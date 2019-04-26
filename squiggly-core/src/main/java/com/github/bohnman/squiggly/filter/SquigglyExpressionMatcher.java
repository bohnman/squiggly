package com.github.bohnman.squiggly.filter;

import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.json.path.CoreJsonPath;
import com.github.bohnman.core.json.path.CoreJsonPathElement;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.BaseSquiggly;
import com.github.bohnman.squiggly.introspect.ObjectDescriptor;
import com.github.bohnman.squiggly.metric.support.CoreCacheMetricsSource;
import com.github.bohnman.squiggly.name.support.AnyDeepName;
import com.github.bohnman.squiggly.name.support.ExactName;
import com.github.bohnman.squiggly.parse.support.ExpressionNode;
import com.github.bohnman.squiggly.parse.support.StatementNode;
import com.github.bohnman.squiggly.view.PropertyView;

import javax.annotation.Nullable;
import java.util.*;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Encapsulates the filter node matching logic.
 */
public class SquigglyExpressionMatcher {

    /**
     * Indicate to never match the path.
     */
    public static final ExpressionNode NEVER_MATCH = ExpressionNode.createNamed(AnyDeepName.get());

    /**
     * Indicate to always match the path.
     */
    public static final ExpressionNode ALWAYS_MATCH = ExpressionNode.createNamed(AnyDeepName.get());

    private static final List<ExpressionNode> BASE_VIEW_NODES = Collections.singletonList(ExpressionNode.createNamedNested(new ExactName(PropertyView.BASE_VIEW)));

    private final CoreCache<CorePair<CoreJsonPath, String>, ExpressionNode> matchCache;
    private final BaseSquiggly squiggly;


    /**
     * Constructor.
     *
     * @param squiggly configurator
     */
    public SquigglyExpressionMatcher(BaseSquiggly squiggly) {
        this.squiggly = notNull(squiggly);
        this.matchCache = CoreCacheBuilder.from(squiggly.getConfig().getFilterPathCacheSpec()).build();
        squiggly.getMetrics().add(new CoreCacheMetricsSource("squiggly.filter.path-cache.", matchCache));
    }

    /**
     * Perform the matching using a context.
     *
     * @param path    the path that is being matched
     * @param context the context holding the root node
     * @return matched node or {@link #ALWAYS_MATCH} or {@link #NEVER_MATCH}
     */
    public ExpressionNode match(CoreJsonPath path, String filter, StatementNode statement) {
        return match(path, filter, statement.getRoot());
    }

    /**
     * Perform the matching using the given node.
     *
     * @param path   that thst is beign matched
     * @param filter the filter string
     * @param node   the root node
     * @return matched node or {@link #ALWAYS_MATCH} or {@link #NEVER_MATCH}
     */
    public ExpressionNode match(CoreJsonPath path, String filter, ExpressionNode node) {
        if (AnyDeepName.ID.equals(filter)) {
            return ALWAYS_MATCH;
        }

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            CorePair<CoreJsonPath, String> pair = CorePair.of(path, filter);
            ExpressionNode match = matchCache.get(pair);

            if (match == null) {
                match = matchInternal(path, node);
            }

            matchCache.put(pair, match);
            return match;
        }

        return matchInternal(path, node);
    }

    private ExpressionNode matchInternal(CoreJsonPath path, ExpressionNode parent) {
        MatchContext context = new MatchContext(path, parent);
        ExpressionNode match = null;

        int pathSize = path.getElements().size();
        int lastIdx = pathSize - 1;

        for (int i = 0; i < pathSize; i++) {
            CoreJsonPathElement element = path.getElements().get(i);

            if (context.viewNode != null && !context.viewNode.isNested()) {
                ExpressionNode viewMatch = matchPropertyName(context, element);

                if (viewMatch == NEVER_MATCH) {
                    return viewMatch;
                }

                if (viewMatch != null) {
                    match = viewMatch;
                }

                continue;
            }

            if (context.nodes.isEmpty()) {
                return NEVER_MATCH;
            }

            match = findBestNode(context, element, pathSize);

            if (match == null && isJsonUnwrapped(element)) {
                match = ALWAYS_MATCH;
            }

            if (match == null) {
                return NEVER_MATCH;
            }

            if (match.isAnyDeep()) {
                return match;
            }

            if (match.isNegated()) {
                return NEVER_MATCH;
            }

            if (i < lastIdx) {
                context.descend(match);
            }
        }

        if (match == null) {
            match = NEVER_MATCH;
        }

        return match;
    }

    private ExpressionNode findBestNode(MatchContext context, CoreJsonPathElement element, int pathSize) {
        ExpressionNode match = findBestSimpleNode(context, element);

        if (match == null) {
            match = findBestViewNode(context, element);
        }

        return match;
    }

    private ExpressionNode matchPropertyName(MatchContext context, CoreJsonPathElement element) {
        Class beanClass = element.getBeanClass();

        if (beanClass != null && !Map.class.isAssignableFrom(beanClass)) {
            Set<String> propertyNames = getPropertyNamesFromViewStack(element, context.viewStack);

            if (!propertyNames.contains(element.getName())) {
                return NEVER_MATCH;
            }
        }

        return null;
    }


    private boolean isJsonUnwrapped(CoreJsonPathElement element) {
        ObjectDescriptor info = squiggly.getObjectIntrospector().introspect(element.getBeanClass());
        return info.isUnwrapped(element.getName());
    }

    private Set<String> getPropertyNamesFromViewStack(CoreJsonPathElement element, Set<String> viewStack) {
        if (viewStack == null) {
            return getPropertyNames(element, PropertyView.BASE_VIEW);
        }

        Set<String> propertyNames = new HashSet<>();

        for (String viewName : viewStack) {
            Set<String> names = getPropertyNames(element, viewName);

            if (names.isEmpty() && squiggly.getConfig().isFilterImplicitlyIncludeBaseFields()) {
                names = getPropertyNames(element, PropertyView.BASE_VIEW);
            }

            propertyNames.addAll(names);
        }

        return propertyNames;
    }

    private ExpressionNode findBestViewNode(MatchContext context, CoreJsonPathElement element) {
        ExpressionNode match = null;

        if (Map.class.isAssignableFrom(element.getBeanClass())) {
            for (ExpressionNode node : context.nodes) {
                if (PropertyView.BASE_VIEW.equals(node.getName())) {
                    match = node;
                    break;
                }
            }
        } else {
            for (ExpressionNode node : context.nodes) {
                // handle view
                Set<String> propertyNames = getPropertyNames(element, node.getName());

                if (propertyNames.contains(element.getName())) {
                    match = node;
                    break;
                }
            }
        }

        if (match != null) {
            context.viewNode = match;
            addToViewStack(context);
        }

        return match;
    }

    private ExpressionNode findBestSimpleNode(MatchContext context, CoreJsonPathElement element) {
        ExpressionNode match = null;

        for (ExpressionNode node : context.nodes) {

            if (!node.matches(element.getName())) {
                continue;
            }

            if (match == null || node.compareTo(match) >= 0) {
                match = node;
            }
        }

        return match;
    }

    private void addToViewStack(MatchContext context) {
        if (!squiggly.getConfig().isFilterPropagateViewToNestedFilters()) {
            return;
        }

        if (context.viewStack == null) {
            context.viewStack = new HashSet<>();
        }

        context.viewStack.add(context.viewNode.getName());
    }

    private Set<String> getPropertyNames(CoreJsonPathElement element, String viewName) {
        Class beanClass = element.getBeanClass();

        if (beanClass == null) {
            return Collections.emptySet();
        }

        return squiggly.getObjectIntrospector().introspect(beanClass).getPropertyNamesForView(viewName);
    }

    private class MatchContext {
        private int depth = 0;
        private int lastIndex;
        private CoreJsonPath path;

        @Nullable
        private Set<String> viewStack;
        private ExpressionNode parent;

        @Nullable
        private ExpressionNode viewNode;
        private List<ExpressionNode> nodes;

        public MatchContext(CoreJsonPath path, ExpressionNode parent) {
            this.path = path;
            this.nodes = Collections.emptyList();
            this.parent = null;
            this.lastIndex = path.getElements().size() - 1;
            descend(parent);
        }

        public boolean isViewNodeSquiggly() {
            return viewNode != null && viewNode.isNested();
        }

        public void descend(ExpressionNode parent) {
            int newDepth = depth + 1;

            nodes = descendNodes(parent, newDepth);


            if (includeBaseViewNodes(parent, newDepth)) {
                nodes = BASE_VIEW_NODES;
            }

//            if (nodes.isEmpty() && !parent.isEmptyNested()) {
//                nodes = previousDeepNodes;
//            } else {
//                nodes = Stream.concat(nodes.stream(), previousDeepNodes.stream()).collect(Collectors.toList());
//            }

            this.depth = newDepth;
            this.parent = parent;
        }

        private boolean includeBaseViewNodes(ExpressionNode parent, int newDepth) {

            if (newDepth < 1) {
                return false;
            }

            if (depth > lastIndex) {
                return false;
            }

            if (!nodes.isEmpty()) {
                return false;
            }

            if (parent.isDeep()) {
                return false;
            }

            if (parent.isEmptyNested()) {
                return false;
            }

            return squiggly.getConfig().isFilterImplicitlyIncludeBaseFields();
        }

        private List<ExpressionNode> descendNodes(ExpressionNode parent, int newDepth) {

            List<ExpressionNode> childNodes = parent.getChildren();
            List<ExpressionNode> previousNodes = nodes;

            List<ExpressionNode> newNodes = new ArrayList<>(previousNodes.size() + childNodes.size());

            boolean deepInherit = false;


            for (ExpressionNode child : childNodes) {
                if (child.isDeepInherit()) {
                    deepInherit = true;
                } else if (child.isAvailableAtDepth(newDepth)) {
                    newNodes.add(child);
                }
            }

            if (!previousNodes.isEmpty() && (deepInherit || newNodes.isEmpty())) {
                for (ExpressionNode previousNode : previousNodes) {
                    if (previousNode.isAvailableAtDepth(newDepth)) {
                        newNodes.add(previousNode);
                    }
                }

            }

            return newNodes;
        }
    }


}

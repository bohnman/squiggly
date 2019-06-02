package com.github.bohnman.squiggly.match.support;

import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.environment.SquigglyEnvironmentOld;
import com.github.bohnman.squiggly.introspect.ObjectDescriptor;
import com.github.bohnman.squiggly.introspect.ObjectIntrospector;
import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.metric.support.CoreCacheMetricsSource;
import com.github.bohnman.squiggly.metric.support.SquigglyMetrics;
import com.github.bohnman.squiggly.name.SquigglyNames;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.path.support.DefaultObjectPath;
import com.github.bohnman.squiggly.path.SquigglyObjectPath;
import com.github.bohnman.squiggly.path.SquigglyObjectPathElement;
import com.github.bohnman.squiggly.view.PropertyView;

import javax.annotation.Nullable;
import java.util.*;

import static java.util.Objects.requireNonNull;

/**
 * Encapsulates the filter node matching logic.
 */
public class DefaultExpressionMatcher implements SquigglyExpressionMatcher {

    private static final List<ExpressionNode> BASE_VIEW_NODES = Collections.singletonList(ExpressionNode.createNamedNested(new SquigglyNames.ExactName(PropertyView.BASE_VIEW)));

    private final SquigglyEnvironmentOld config;
    private final CoreCache<CorePair<SquigglyObjectPath, String>, ExpressionNode> matchCache;
    private final ObjectIntrospector introspector;

    /**
     * Constructor.
     *
     * @param squiggly configurator
     */
    public DefaultExpressionMatcher(
            SquigglyEnvironmentOld environment,
            ObjectIntrospector introspector,
            SquigglyMetrics metrics) {

        this.config = requireNonNull(environment);
        this.matchCache = CoreCacheBuilder.from(environment.getFilterPathCacheSpec()).build();
        this.introspector = requireNonNull(introspector);
        metrics.add(new CoreCacheMetricsSource("squiggly.filter.path-cache.", matchCache));
    }


    /**
     * Perform the matching using the given node.
     *
     * @param path   that thst is beign matched
     * @param filter the filter string
     * @param expression   the root node
     * @return matched node or {@link #ALWAYS_MATCH} or {@link #NEVER_MATCH}
     */
    public ExpressionNode match(DefaultObjectPath path, String filter, ExpressionNode expression) {
        if (SquigglyNames.AnyDeepName.ANY_DEEP_SYMBOL.equals(filter)) {
            return ALWAYS_MATCH;
        }


        if (isCacheable(path)) {
            // cache the match result using the path and filter expression
            CorePair<SquigglyObjectPath, String> pair = CorePair.of(path, filter);
            ExpressionNode match = matchCache.get(pair);

            if (match == null) {
                match = matchInternal(path, expression);
            }

            matchCache.put(pair, match);
            return match;
        }

        return matchInternal(path, expression);
    }

    private boolean isCacheable(DefaultObjectPath path) {
        SquigglyObjectPathElement last = path.getLast();
        Class<?> objectClass = last == null ? null : last.getObjectClass();
        return objectClass != null && !Map.class.isAssignableFrom(objectClass);
    }

    private ExpressionNode matchInternal(DefaultObjectPath path, ExpressionNode parent) {
        MatchContext context = new MatchContext(path, parent);
        ExpressionNode match = null;

        int pathSize = path.getElements().size();
        int lastIdx = pathSize - 1;

        for (int i = 0; i < pathSize; i++) {
            SquigglyObjectPathElement element = path.getElements().get(i);

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

    private ExpressionNode findBestNode(MatchContext context, SquigglyObjectPathElement element, int pathSize) {
        ExpressionNode match = findBestSimpleNode(context, element);

        if (match == null) {
            match = findBestViewNode(context, element);
        }

        return match;
    }

    private ExpressionNode matchPropertyName(MatchContext context, SquigglyObjectPathElement element) {
        Class<?> beanClass = element.getObjectClass();

        if (beanClass != null && !Map.class.isAssignableFrom(beanClass)) {
            Set<String> propertyNames = getPropertyNamesFromViewStack(element, context.viewStack);

            if (!propertyNames.contains(element.getName())) {
                return NEVER_MATCH;
            }
        }

        return null;
    }


    private boolean isJsonUnwrapped(SquigglyObjectPathElement element) {
        ObjectDescriptor info = introspector.introspect(element.getObjectClass());
        return info.isUnwrapped(element.getName());
    }

    private Set<String> getPropertyNamesFromViewStack(SquigglyObjectPathElement element, Set<String> viewStack) {
        if (viewStack == null) {
            return getPropertyNames(element, PropertyView.BASE_VIEW);
        }

        Set<String> propertyNames = new HashSet<>();

        for (String viewName : viewStack) {
            Set<String> names = getPropertyNames(element, viewName);

            if (names.isEmpty() && config.isFilterImplicitlyIncludeBaseFields()) {
                names = getPropertyNames(element, PropertyView.BASE_VIEW);
            }

            propertyNames.addAll(names);
        }

        return propertyNames;
    }

    private ExpressionNode findBestViewNode(MatchContext context, SquigglyObjectPathElement element) {
        ExpressionNode match = null;

        if (Map.class.isAssignableFrom(element.getObjectClass())) {
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

    private ExpressionNode findBestSimpleNode(MatchContext context, SquigglyObjectPathElement element) {
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
        if (!config.isFilterPropagateViewToNestedFilters()) {
            return;
        }

        if (context.viewStack == null) {
            context.viewStack = new HashSet<>();
        }

        context.viewStack.add(context.viewNode.getName());
    }

    private Set<String> getPropertyNames(SquigglyObjectPathElement element, String viewName) {
        Class<?> beanClass = element.getObjectClass();

        if (beanClass == null) {
            return Collections.emptySet();
        }

        return introspector.introspect(beanClass).getPropertyNamesForView(viewName);
    }

    private class MatchContext {
        private int depth = 0;
        private int lastIndex;
        private SquigglyObjectPath path;

        @Nullable
        private Set<String> viewStack;
        private ExpressionNode parent;

        @Nullable
        private ExpressionNode viewNode;
        private List<ExpressionNode> nodes;

        public MatchContext(DefaultObjectPath path, ExpressionNode parent) {
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

            return config.isFilterImplicitlyIncludeBaseFields();
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

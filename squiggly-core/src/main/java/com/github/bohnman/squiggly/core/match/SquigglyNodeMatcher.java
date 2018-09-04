package com.github.bohnman.squiggly.core.match;

import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.json.path.CoreJsonPath;
import com.github.bohnman.core.json.path.CoreJsonPathElement;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.bean.BeanInfo;
import com.github.bohnman.squiggly.core.context.SquigglyContext;
import com.github.bohnman.squiggly.core.metric.source.CoreCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.core.name.AnyDeepName;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.parser.node.SquigglyNode;
import com.github.bohnman.squiggly.core.view.PropertyView;

import java.util.*;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Encapsulates the filter node matching logic.
 */
public class SquigglyNodeMatcher {

    /**
     * Indicate to never match the path.
     */
    public static final SquigglyNode NEVER_MATCH = SquigglyNode.createNamed(AnyDeepName.get());

    /**
     * Indicate to always match the path.
     */
    public static final SquigglyNode ALWAYS_MATCH = SquigglyNode.createNamed(AnyDeepName.get());

    private static final List<SquigglyNode> BASE_VIEW_NODES = Collections.singletonList(SquigglyNode.createNamedSquiggly(new ExactName(PropertyView.BASE_VIEW)));

    private final CoreCache<CorePair<CoreJsonPath, String>, SquigglyNode> matchCache;
    private final BaseSquiggly squiggly;


    /**
     * Constructor.
     *
     * @param squiggly configurator
     */
    @SuppressWarnings("unchecked")
    public SquigglyNodeMatcher(BaseSquiggly squiggly) {
        this.squiggly = notNull(squiggly);
        this.matchCache = CoreCacheBuilder.from(squiggly.getConfig().getFilterPathCacheSpec()).build();
        squiggly.getMetrics().add(new CoreCacheSquigglyMetricsSource("squiggly.filter.pathCache.", matchCache));
    }

    /**
     * Perform the matching using a context.
     *
     * @param path    the path that is being matched
     * @param context the context holding the root node
     * @return matched node or {@link #ALWAYS_MATCH} or {@link #NEVER_MATCH}
     */
    @SuppressWarnings("unchecked")
    public SquigglyNode match(CoreJsonPath path, SquigglyContext context) {
        return match(path, context.getFilter(), context.getNode());
    }

    /**
     * Perform the matching using the given node.
     *
     * @param path   that thst is beign matched
     * @param filter the filter string
     * @param node   the root node
     * @return matched node or {@link #ALWAYS_MATCH} or {@link #NEVER_MATCH}
     */
    public SquigglyNode match(CoreJsonPath path, String filter, SquigglyNode node) {
        if (AnyDeepName.ID.equals(filter)) {
            return ALWAYS_MATCH;
        }

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            CorePair<CoreJsonPath, String> pair = CorePair.of(path, filter);
            SquigglyNode match = matchCache.get(pair);

            if (match == null) {
                match = matchInternal(path, node);
            }

            matchCache.put(pair, match);
            return match;
        }

        return matchInternal(path, node);
    }

    private class MatchContext {
        private int depth;
        private int lastIndex;
        private CoreJsonPath path;
        private Set<String> viewStack;
        private SquigglyNode parent;
        private SquigglyNode viewNode;
        private List<SquigglyNode> nodes;
        private List<SquigglyNode> recursiveNodes = Collections.emptyList();

        public MatchContext(CoreJsonPath path, SquigglyNode parent) {
            this.path = path;
            this.parent = parent;
            this.lastIndex = path.getElements().size() - 1;
            this.nodes = parent.getChildren();
        }

        public boolean isViewNodeSquiggly() {
            return viewNode != null && viewNode.isSquiggly();
        }

        public void descend(SquigglyNode parent, int newDepth) {
            nodes = parent.getChildren();

            if (depth < lastIndex && nodes.isEmpty() && !parent.isEmptyNested() && squiggly.getConfig().isFilterImplicitlyIncludeBaseFields()) {
                nodes = BASE_VIEW_NODES;
            }

            this.depth = newDepth;
            this.parent = parent;
        }
    }

    private SquigglyNode matchInternal(CoreJsonPath path, SquigglyNode parent) {
        MatchContext context = new MatchContext(path, parent);
        SquigglyNode match = null;

        int pathSize = path.getElements().size();
        int lastIdx = pathSize - 1;


        for (int i = 0; i < pathSize; i++) {
            int depth = i;
            CoreJsonPathElement element = path.getElements().get(i);

            if (context.viewNode != null && !context.viewNode.isSquiggly()) {
                SquigglyNode viewMatch = matchPropertyName(context, element);

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

            match = findBestNode(context, element);

            if (match == null && isJsonUnwrapped(element)) {
                match = ALWAYS_MATCH;
                continue;
            }

            if (match == null) {
                return NEVER_MATCH;
            }

            if (match.isAnyDeep()) {
                return match;
            }

            if (match.isAnyShallow()) {
                context.viewNode = match;
            }

            if (match.isNegated()) {
                return NEVER_MATCH;
            }

            context.descend(match, depth + 1);
        }

        if (match == null) {
            match = NEVER_MATCH;
        }

        return match;
    }

    private SquigglyNode findBestNode(MatchContext context, CoreJsonPathElement element) {
        SquigglyNode match = findBestSimpleNode(context, element);

        if (match == null) {
            match = findBestViewNode(context, element);
        }

        return match;
    }

    private SquigglyNode matchPropertyName(MatchContext context, CoreJsonPathElement element) {
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
        BeanInfo info = squiggly.getBeanInfoIntrospector().introspect(element.getBeanClass());
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

    private SquigglyNode findBestViewNode(MatchContext context, CoreJsonPathElement element) {
        SquigglyNode match = null;

        if (Map.class.isAssignableFrom(element.getBeanClass())) {
            for (SquigglyNode node : context.nodes) {
                if (PropertyView.BASE_VIEW.equals(node.getName())) {
                    match = node;
                    break;
                }
            }
        } else {
            for (SquigglyNode node : context.nodes) {
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

    private SquigglyNode findBestSimpleNode(MatchContext context, CoreJsonPathElement element) {
        SquigglyNode match = null;
        int lastMatchStrength = -1;

        for (SquigglyNode node : context.nodes) {
            int matchStrength = node.match(element.getName());

            if (matchStrength < 0) {
                continue;
            }

            if (lastMatchStrength < 0 || matchStrength >= lastMatchStrength) {
                match = node;
                lastMatchStrength = matchStrength;
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

        return squiggly.getBeanInfoIntrospector().introspect(beanClass).getPropertyNamesForView(viewName);
    }


}

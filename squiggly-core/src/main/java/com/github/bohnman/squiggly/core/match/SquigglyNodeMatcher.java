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
import com.github.bohnman.squiggly.core.parse.node.SquigglyNode;
import com.github.bohnman.squiggly.core.view.PropertyView;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class SquigglyNodeMatcher {

    public static final SquigglyNode NEVER_MATCH = SquigglyNode.builder().build();
    public static final SquigglyNode ALWAYS_MATCH = SquigglyNode.builder().build();
    private static final List<SquigglyNode> BASE_VIEW_NODES = Collections.singletonList(SquigglyNode.builder().name(new ExactName(PropertyView.BASE_VIEW)).build());

    private final CoreCache<CorePair<CoreJsonPath, String>, SquigglyNode> matchCache;
    private final BaseSquiggly squiggly;


    @SuppressWarnings("unchecked")
    public SquigglyNodeMatcher(BaseSquiggly squiggly) {
        this.squiggly = notNull(squiggly);
        this.matchCache = CoreCacheBuilder.from(squiggly.getConfig().getFilterPathCacheSpec()).build();
        squiggly.getMetrics().add(new CoreCacheSquigglyMetricsSource("squiggly.filter.pathCache.", matchCache));
    }

    @SuppressWarnings("unchecked")

    public SquigglyNode match(CoreJsonPath path, SquigglyContext context) {
        return match(path, context.getFilter(), context.getNode());
    }

    public SquigglyNode match(CoreJsonPath path, String filter, SquigglyNode root) {
        if (AnyDeepName.ID.equals(filter)) {
            return ALWAYS_MATCH;
        }

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            CorePair<CoreJsonPath, String> pair = CorePair.of(path, filter);
            SquigglyNode match = matchCache.get(pair);

            if (match == null) {
                match = matchInternal(path, root);
            }

            matchCache.put(pair, match);
            return match;
        }

        return matchInternal(path, root);
    }

    private SquigglyNode matchInternal(CoreJsonPath path, SquigglyNode root) {
        return new Matcher(path, root).match();
    }


    private boolean isJsonUnwrapped(CoreJsonPathElement element) {
        BeanInfo info = squiggly.getBeanInfoIntrospector().introspect(element.getBeanClass());
        return info.isUnwrapped(element.getName());
    }


    private Set<String> addToViewStack(Set<String> viewStack, SquigglyNode viewNode) {
        if (!squiggly.getConfig().isFilterPropagateViewToNestedFilters()) {
            return null;
        }

        if (viewStack == null) {
            viewStack = new HashSet<>();
        }

        viewStack.add(viewNode.getName());

        return viewStack;
    }


    private class Matcher {
        private final CoreJsonPath path;
        private List<SquigglyNode> nodes;
        private List<SquigglyNode> recursiveNodes;
        private Set<String> viewStack;
        private SquigglyNode viewNode;

        public Matcher(CoreJsonPath path, SquigglyNode root) {
            this.path = path;
            descend(root, 0);
        }

        private void descend(SquigglyNode parent, int depth) {
            List<SquigglyNode> children = parent.getChildren();
            int size = children.size();
            int recursiveIndex = -1;

            if (recursiveNodes != null) {
                recursiveNodes.removeIf(squigglyNode -> !includesDepth(squigglyNode, depth));
            }

            for (int i = 0; i < size; i++) {
                SquigglyNode node = children.get(i);

                if (node.isRecursive()) {
                    recursiveIndex = i;
                    break;
                }
            }

            if (recursiveIndex >= 0) {
                children = new ArrayList<>(children.size());

                if (recursiveNodes == null) {
                    recursiveNodes = new LinkedList<>();
                }

                for (int i = 0; i < recursiveIndex; i++) {
                    children.add(parent.getChildren().get(i));
                }

                if (includesDepth(parent.getChildren().get(recursiveIndex), depth)) {
                    recursiveNodes.add(parent.getChildren().get(recursiveIndex));
                }

                for (int i = recursiveIndex + 1; i < size; i++) {
                    SquigglyNode node = parent.getChildren().get(i);

                    if (!node.isRecursive()) {
                        children.add(node);
                    } else if (includesDepth(node, depth)) {
                        recursiveNodes.add(node);
                    }
                }
            }

            nodes = children;
        }

        private boolean includesDepth(SquigglyNode node, int depth) {
            Integer startDepth = node.getStartDepth();
            Integer endDepth = node.getEndDepth();

            if (startDepth != null && startDepth > -1 && startDepth > depth) {
                return false;
            }

            if (endDepth != null && endDepth > -1 && depth >= endDepth) {
                return false;
            }

            return true;
        }


        public SquigglyNode match() {
            int pathSize = path.getElements().size();
            int lastIdx = pathSize - 1;
            SquigglyNode match = null;

            for (int i = 0; i < pathSize; i++) {
                CoreJsonPathElement element = path.getElements().get(i);

                if (viewNode != null && !viewNode.isSquiggly()) {
                    if (!handleViewNode(element)) {
                        return NEVER_MATCH;
                    }
                } else if (nodesIsEmpty()) {
                    return NEVER_MATCH;
                } else {
                    match = findBestNode(element, i);

                    if (match == null) {
                        if (isJsonUnwrapped(element)) {
                            match = ALWAYS_MATCH;
                            continue;
                        }

                        return NEVER_MATCH;
                    }

                    if (match.isAnyDeep()) {
                        return match;
                    }

                    if (match.isNegated()) {
                        return NEVER_MATCH;
                    }

                    descend(match, i + 1);

                    if (shouldUseBaseViewNodes(match, i, lastIdx)) {
                        nodes = BASE_VIEW_NODES;
                    }
                }
            }

            if (match == null) {
                return NEVER_MATCH;
            }

            return match;
        }

        private SquigglyNode findBestNode(CoreJsonPathElement element, int depth) {
            SquigglyNode match = findBestNode(element, depth, nodes);

            SquigglyNode recursiveMatch = findBestNode(element, depth, recursiveNodes);

            if (match == null) {
                return recursiveMatch;
            }

            if (recursiveMatch == null) {
                return match;
            }

            if (recursiveMatch.match(element.getName()) > match.match(element.getName())) {
                return null;
            }

            return match;
        }

        private SquigglyNode findBestNode(CoreJsonPathElement element, int depth, List<SquigglyNode> nodes) {

            if (nodes == null) {
                return null;
            }

            SquigglyNode match = findBestSimpleNode(element, nodes);

            if (match == null) {
                match = findBestViewNode(element, nodes);

                if (match != null) {
                    viewNode = match;
                    viewStack = addToViewStack(viewStack, viewNode);
                }
            }/* else if (match.isAnyShallow()) {
                viewNode = match;
            }*/

            return match;
        }

        private boolean handleViewNode(CoreJsonPathElement element) {
            Class beanClass = element.getBeanClass();

            if (beanClass == null) {
                return true;
            }

            if (Map.class.isAssignableFrom(beanClass)) {
                return true;
            }

            Set<String> propertyNames = getPropertyNamesFromViewStack(element);
            return propertyNames.contains(element.getName());
        }

        private boolean shouldUseBaseViewNodes(SquigglyNode match, int i, int lastIdx) {
            if (i >= lastIdx) {
                return false;
            }

            if (!nodesIsEmpty()) {
                return false;
            }

            if (match.isRecursive()) {
                return false;
            }

            if (match.isEmptyNested()) {
                return false;
            }

            return squiggly.getConfig().isFilterImplicitlyIncludeBaseFields();
        }

        private boolean nodesIsEmpty() {
            return nodes.isEmpty() && (recursiveNodes == null || recursiveNodes.isEmpty());
        }

        private Set<String> getPropertyNamesFromViewStack(CoreJsonPathElement element) {
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

        private Set<String> getPropertyNames(CoreJsonPathElement element, String viewName) {
            Class beanClass = element.getBeanClass();

            if (beanClass == null) {
                return Collections.emptySet();
            }

            return squiggly.getBeanInfoIntrospector().introspect(beanClass).getPropertyNamesForView(viewName);
        }

        private SquigglyNode findBestViewNode(CoreJsonPathElement element, List<SquigglyNode> nodes) {
            if (Map.class.isAssignableFrom(element.getBeanClass())) {
                for (SquigglyNode node : nodes) {
                    if (PropertyView.BASE_VIEW.equals(node.getName())) {
                        return node;
                    }
                }
            } else {
                for (SquigglyNode node : nodes) {
                    // handle view
                    Set<String> propertyNames = getPropertyNames(element, node.getName());

                    if (propertyNames.contains(element.getName())) {
                        return node;
                    }
                }
            }

            return null;
        }

        private SquigglyNode findBestSimpleNode(CoreJsonPathElement element, List<SquigglyNode> nodes) {
            SquigglyNode match = null;
            int lastMatchStrength = -1;

            for (SquigglyNode node : nodes) {
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

    }
}

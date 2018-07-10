package com.github.bohnman.squiggly.core.filter;

import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.context.SquigglyContext;
import com.github.bohnman.squiggly.core.match.SquigglyNodeMatcher;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;

import java.util.List;
import java.util.Objects;

/**
 * Base class for filtering json nodes.
 */
@SuppressWarnings("unchecked")
public class SquigglyNodeFilter {

    private final BaseSquiggly squiggly;

    /**
     * Constructor.
     *
     * @param squiggly the squiggly configurator
     */
    public SquigglyNodeFilter(BaseSquiggly squiggly) {
        this.squiggly = CoreAssert.notNull(squiggly);
    }

    /**
     * Apply the supplied filters to the node returning the filtered node.
     *
     * @param node    json node
     * @param filters filters to apply
     * @param <T>     node type
     * @return filtered node
     */
    public <T> CoreJsonNode<T> apply(CoreJsonNode<T> node, String... filters) {
        for (String filter : filters) {
            node = applyFilter(node, filter);
        }

        if (appendContextFilter() && squiggly.getContextProvider().isFilteringEnabled()) {
            Object value = node.getValue();
            Class<?> beanClass = value == null ? Object.class : value.getClass();
            SquigglyContext context = squiggly.getContextProvider().getContext(beanClass, squiggly);
            node = applyFilter(node, context.getFilter(), context.getNode());
        }

        return node;
    }

    /**
     * Hook method to determine if the context filter should be applied.
     *
     * @return true if apply
     */
    protected boolean appendContextFilter() {
        return CoreObjects.firstNonNull(squiggly.getConfig().getAppendContextInNodeFilter(), true);
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> node, String filter) {
        List<SquigglyNode> squigglyNodes = squiggly.getParser().parseNodeFilter(filter);

        for (SquigglyNode squigglyNode : squigglyNodes) {
            node = applyFilter(node, filter, squiggly.getNodeNormalizer().normalize(squigglyNode));
        }

        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> rootJsonNode, String filter, SquigglyNode squigglyNode) {
        if (squigglyNode == null) {
            return rootJsonNode;
        }

        if (squigglyNode.isAnyDeep()) {
            return invokeValueFunctions(rootJsonNode, rootJsonNode, squigglyNode);
        }

        return rootJsonNode.transform((context, jsonNode) -> {
            if (context.getObjectPath().isEmpty()) {
                if (squigglyNode.getValueFunctions().isEmpty()) {
                    return jsonNode;
                }

                return invokeValueFunctions(jsonNode, jsonNode, squigglyNode);
            }

            if (context.getKey() instanceof Number) {
                // skip, because we're at the array element level
                return jsonNode;
            }

            SquigglyNode match = squiggly.getNodeMatcher().match(context.getObjectPath(), filter, squigglyNode);

            if (match == null || match == SquigglyNodeMatcher.NEVER_MATCH) {
                return null;
            }

            context.setKey(Objects.toString(squiggly.getFunctionInvoker().invoke(context.getKey(), context.getParentNode(), match.getKeyFunctions())));
            return invokeValueFunctions(jsonNode, context.getParentNode(), match);
        });
    }

    private <T> CoreJsonNode<T> invokeValueFunctions(CoreJsonNode<T> jsonNode, CoreJsonNode parentNode, SquigglyNode squigglyNode) {
        Object newValue = squiggly.getFunctionInvoker().invoke(jsonNode, parentNode, squigglyNode.getValueFunctions());

        if (newValue instanceof CoreJsonNode) {
            return (CoreJsonNode) newValue;
        }

        return jsonNode.create(newValue);
    }
}

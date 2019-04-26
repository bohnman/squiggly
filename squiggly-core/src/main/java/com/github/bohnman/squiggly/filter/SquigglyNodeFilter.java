package com.github.bohnman.squiggly.filter;

import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.BaseSquiggly;
import com.github.bohnman.squiggly.parse.support.ExpressionNode;
import com.github.bohnman.squiggly.parse.support.FilterNode;
import com.github.bohnman.squiggly.parse.support.StatementNode;

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
            SquigglyFilterContext context = squiggly.getContextProvider().getContext(beanClass, squiggly);
            node = applyFilter(node, context.getFilter(), context.getParsedFilter());
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
        return applyFilter(node, filter, squiggly.getParser().parseNodeFilter(filter));
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> node, String filter, FilterNode filterNode) {
        for (StatementNode statement : filterNode.getStatements()) {
            node = applyFilter(node, filter, statement);
        }

        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> rootJsonNode, String filter, StatementNode statement) {
        return applyFilter(rootJsonNode, filter, statement.getRoot());
    }


    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> rootJsonNode, String filter, ExpressionNode expression) {
        if (expression == null) {
            return rootJsonNode;
        }

        if (expression.isAnyDeep()) {
            return invokeValueFunctions(rootJsonNode, rootJsonNode, expression);
        }

        return rootJsonNode.transform((context, jsonNode) -> {
            if (context.getObjectPath().isEmpty()) {
                if (expression.getValueFunctions().isEmpty()) {
                    return jsonNode;
                }

                return invokeValueFunctions(jsonNode, jsonNode, expression);
            }

            if (context.getKey() instanceof Number) {
                // skip, because we're at the array element level
                return jsonNode;
            }

            ExpressionNode match = squiggly.getExpressionMatcher().match(context.getObjectPath(), filter, expression);

            if (match == null || match == SquigglyExpressionMatcher.NEVER_MATCH) {
                return null;
            }

            context.setKey(Objects.toString(squiggly.getFunctionInvoker().invoke(context.getKey(), context.getKey(), context.getParentNode(), match.getKeyFunctions())));
            return invokeValueFunctions(jsonNode, context.getParentNode(), match);
        });
    }

    private <T> CoreJsonNode<T> invokeValueFunctions(CoreJsonNode<T> jsonNode, CoreJsonNode parentNode, ExpressionNode expression) {
        Object newValue = squiggly.getFunctionInvoker().invoke(jsonNode, jsonNode, parentNode, expression.getValueFunctions());

        if (newValue instanceof CoreJsonNode) {
            return (CoreJsonNode) newValue;
        }

        return jsonNode.create(newValue);
    }
}

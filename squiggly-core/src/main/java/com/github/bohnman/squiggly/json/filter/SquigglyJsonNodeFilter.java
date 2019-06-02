package com.github.bohnman.squiggly.json.filter;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.environment.SquigglyEnvironmentOld;
import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.match.support.DefaultExpressionMatcher;
import com.github.bohnman.squiggly.filter.SquigglyFilterContext;
import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.function.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNode;
import com.github.bohnman.squiggly.parse.SquigglyParser;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.node.FilterNode;
import com.github.bohnman.squiggly.node.StatementNode;

import java.util.Objects;

/**
 * Base class for filtering json nodes.
 */
@SuppressWarnings("unchecked")
public class SquigglyJsonNodeFilter {

    private final SquigglyEnvironmentOld config;
    private final DefaultExpressionMatcher expressionMatcher;
    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyParser parser;

    public SquigglyJsonNodeFilter(
            SquigglyEnvironmentOld config,
            DefaultExpressionMatcher expressionMatcher,
            SquigglyFilterContextProvider filterContextProvider,
            SquigglyFilterSource filterSource,
            SquigglyFunctionInvoker functionInvoker,
            SquigglyParser parser) {
        this.config = config;
        this.expressionMatcher = expressionMatcher;
        this.filterContextProvider = filterContextProvider;
        this.functionInvoker = functionInvoker;
        this.parser = parser;
    }

    /**
     * Apply the supplied filters to the node returning the filtered node.
     *
     * @param node    json node
     * @param filters filters to apply
     * @param <T>     node type
     * @return filtered node
     */
    public <T> SquigglyJsonNode<T> apply(SquigglyJsonNode<T> node, String... filters) {
        for (String filter : filters) {
            node = applyFilter(node, filter);
        }

        if (appendContextFilter()) {
            Object value = node.getValue();
            Class<?> beanClass = value == null ? Object.class : value.getClass();
            SquigglyFilterContext context = filterContextProvider.getContext(beanClass);

            if (context.getFilter() != null) {
                node = applyFilter(node, context.getFilter(), context.getFilterNode());
            }
        }

        return node;
    }

    /**
     * Hook method to determine if the context filter should be applied.
     *
     * @return true if apply
     */
    protected boolean appendContextFilter() {
        return CoreObjects.firstNonNull(config.getAppendContextInNodeFilter(), true);
    }

    private <T> SquigglyJsonNode<T> applyFilter(SquigglyJsonNode<T> node, String filter) {
        return applyFilter(node, filter, parser.parseNodeFilter(filter));
    }

    private <T> SquigglyJsonNode<T> applyFilter(SquigglyJsonNode<T> node, String filter, FilterNode filterNode) {
        for (StatementNode statement : filterNode.getStatements()) {
            node = applyFilter(node, filter, statement);
        }

        return node;
    }

    private <T> SquigglyJsonNode<T> applyFilter(SquigglyJsonNode<T> rootJsonNode, String filter, StatementNode statement) {
        return applyFilter(rootJsonNode, filter, statement.getRoot());
    }


    private <T> SquigglyJsonNode<T> applyFilter(SquigglyJsonNode<T> rootJsonNode, String filter, ExpressionNode expression) {
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

            ExpressionNode match = expressionMatcher.match(context.getObjectPath(), filter, expression);

            if (match == null || match == SquigglyExpressionMatcher.NEVER_MATCH) {
                return null;
            }

            context.setKey(Objects.toString(functionInvoker.invoke(context.getKey(), context.getKey(), context.getParentNode(), match.getKeyFunctions())));
            return invokeValueFunctions(jsonNode, context.getParentNode(), match);
        });
    }

    private <T> SquigglyJsonNode<T> invokeValueFunctions(SquigglyJsonNode<T> jsonNode, SquigglyJsonNode parentNode, ExpressionNode expression) {
        Object newValue = functionInvoker.invoke(jsonNode, jsonNode, parentNode, expression.getValueFunctions());

        if (newValue instanceof SquigglyJsonNode) {
            return (SquigglyJsonNode) newValue;
        }

        return jsonNode.create(newValue);
    }
}

package com.github.bohnman.squiggly.core.filter;

import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.context.SquigglyContext;
import com.github.bohnman.squiggly.core.match.SquigglyNodeMatcher;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;

import java.util.List;
import java.util.Objects;

@SuppressWarnings("unchecked")
public class SquigglyNodeFilter {

    private final BaseSquiggly squiggly;

    public SquigglyNodeFilter(BaseSquiggly squiggly) {
        this.squiggly = CoreAssert.notNull(squiggly);
    }


    public <T> CoreJsonNode<T> apply(CoreJsonNode<T> node, String... filters) {
        for (String filter : filters) {
            node = applyFilter(node, filter);
        }

        if (squiggly.getConfig().isUseContextInNodeFilter() && squiggly.getContextProvider().isFilteringEnabled()) {
            Object value = node.getValue();
            Class<?> beanClass = value == null ? Object.class : value.getClass();
            SquigglyContext context = squiggly.getContextProvider().getContext(beanClass, squiggly);
            node = applyFilter(node, context.getFilter(), context.getNode());
        }

        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> node, String filter) {
        List<SquigglyNode> squigglyNodes = squiggly.getParser().parseNodeFilter(filter);
        for (SquigglyNode squigglyNode : squigglyNodes) {
            node = applyFilter(node, filter, squigglyNode);
        }

        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> rootJsonNode, String filter, SquigglyNode squigglyNode) {
        if (squigglyNode == null) {
            return rootJsonNode;
        }

        return rootJsonNode.transform((context, jsonNode) -> {
            if (context.getPath().isEmpty()) {
                return jsonNode;
            }

            SquigglyNode match = squiggly.getNodeMatcher().match(context.getPath(), filter, squigglyNode);

            if (match == null || match == SquigglyNodeMatcher.NEVER_MATCH) {
                return null;
            }

            context.setKey("" + squiggly.getFunctionInvoker().invoke(context.getKey(), match.getKeyFunctions()));
            Object origValue = jsonNode.getValue();
            Object newValue = squiggly.getFunctionInvoker().invoke(origValue, match.getValueFunctions());

            if (Objects.equals(origValue, newValue)) {
                return jsonNode;
            }

            return jsonNode.create(newValue);
        });
    }
}

package com.github.bohnman.squiggly.jackson.filter;

import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.squiggly.core.function.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.jackson.json.node.BaseCoreJsonNodeVisitor;
import com.github.bohnman.squiggly.jackson.json.node.CoreJsonNode;
import com.github.bohnman.squiggly.jackson.json.node.CoreJsonNodeVisitorContext;
import com.github.bohnman.squiggly.jackson.match.SquigglyNodeMatcher;

import java.util.List;
import java.util.Objects;

public class SquigglyNodeFilter {

    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyNodeMatcher nodeMatcher;
    private final SquigglyParser parser;

    public SquigglyNodeFilter(SquigglyParser parser, SquigglyNodeMatcher nodeMatcher, SquigglyFunctionInvoker functionInvoker) {
        this.parser = CoreAssert.notNull(parser);
        this.functionInvoker = CoreAssert.notNull(functionInvoker);
        this.nodeMatcher = CoreAssert.notNull(nodeMatcher);
    }


    public <T> CoreJsonNode<T> apply(CoreJsonNode<T> node, String... filters) {
        for (String filter : filters) {
            node = applyFilter(node, filter);
        }

        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> node, String filter) {
        List<SquigglyNode> squigglyNodes = parser.parseNodeFilter(filter);
        for (SquigglyNode squigglyNode : squigglyNodes) {
            node = applyFilter(node, filter, squigglyNode);
        }
        return node;
    }

    private <T> CoreJsonNode<T> applyFilter(CoreJsonNode<T> rootJsonNode, String filter, SquigglyNode squigglyNode) {
        return new BaseCoreJsonNodeVisitor<T>() {
            @Override
            protected CoreJsonNode<T> visitAtomNode(CoreJsonNodeVisitorContext context, CoreJsonNode<T> jsonNode) {
                if (context.getPath().isEmpty()) {
                    return jsonNode;
                }

                SquigglyNode match = nodeMatcher.match(context.getPath(), filter, squigglyNode);

                if (match == null || match == SquigglyNodeMatcher.NEVER_MATCH) {
                    return null;
                }

                context.setKey("" + functionInvoker.invoke(context.getKey(), match.getKeyFunctions()));
                Object origValue = jsonNode.getValue();
                Object newValue = functionInvoker.invoke(origValue, match.getValueFunctions());

                if (Objects.equals(origValue, newValue)) {
                    return jsonNode;
                }

                return jsonNode.create(newValue);
            }
        }.visit(rootJsonNode);
    }

}

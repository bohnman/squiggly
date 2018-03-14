package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.json.node.BaseCoreJsonNodeVisitor;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNodeType;
import com.github.bohnman.core.json.node.CoreJsonNodeVisitorContext;
import com.github.bohnman.core.lang.CoreObjects;

import java.util.ArrayList;
import java.util.List;

public class CoreJsonNodeFunctions {

    private CoreJsonNodeFunctions() {
    }

    public static <T> CoreJsonNode<T> flatten(CoreJsonNode<T> node) {
        return flatten(node, -1);
    }

    public static <T> CoreJsonNode<T> flatten(CoreJsonNode<T> node, Number maxDepth) {
        if (node.getType() != CoreJsonNodeType.ARRAY) {
            return node;
        }

        maxDepth = CoreObjects.firstNonNull(maxDepth, -1);
        int maxDepthInt = maxDepth.intValue();

        if (maxDepthInt == 0) {
            return node;
        }

        List<CoreJsonNode<T>> childNodes = new ArrayList<>();

        new BaseCoreJsonNodeVisitor<T>() {

            @Override
            protected CoreJsonNode<T> visitArray(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
                for (CoreJsonNode<T> child : node.getArrayElements()) {
                    if (child.isArray() && (maxDepthInt < 0 || ((context.getDepth() + 1) <= maxDepthInt))) {
                        visitArray(context.descend(null, node, null, null), child);
                    } else {
                        childNodes.add(child);
                    }
                }

                return node;
            }

            @Override
            protected CoreJsonNode<T> visitChildren(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
                return node;
            }
        }.visit(node);

        return node.createArray(childNodes);
    }

}

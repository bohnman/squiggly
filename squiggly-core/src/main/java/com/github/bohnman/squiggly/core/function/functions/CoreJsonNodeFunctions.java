package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.json.node.BaseCoreJsonNodeVisitor;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNodeType;
import com.github.bohnman.core.json.node.CoreJsonNodeVisitorContext;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import java.util.ArrayList;
import java.util.List;

public class CoreJsonNodeFunctions {

    private CoreJsonNodeFunctions() {
    }

    public static <T> CoreJsonNode<T> find(CoreJsonNode<T> node, CoreLambda lambda) {
        if (node == null) {
            return null;
        }

        if (lambda == null) {
            return node;
        }

        List<CoreJsonNode<T>> matches = new ArrayList<>();

        new BaseCoreJsonNodeVisitor<T>() {
            @Override
            protected CoreJsonNode<T> visitAtomNode(CoreJsonNodeVisitorContext<T> context, CoreJsonNode<T> node) {
                boolean result = CoreConversions.toBoolean(lambda.invoke(node.getValue(), context.getKey(), context.getDepth(), node, context));

                if (result) {
                    matches.add(node);
                }

                return node;
            }
        }.visit(node);

        return node.createArray(matches);
    }

    @SquigglyFunctionMethod(aliases = {"findAndReplace"})
    @SuppressWarnings("unchecked")
    public static <T> CoreJsonNode transform(CoreJsonNode<T> node, CoreLambda predicate, CoreLambda replacement) {
        if (node == null) {
            return null;
        }

        if (predicate == null || replacement == null) {
            return node;
        }

        return node.transform((context, candidate) -> {
            boolean test = CoreConversions.toBoolean(predicate.invoke(candidate.getValue(), context.getKey(), context.getDepth(), candidate, context));

            if (!test) {
                return candidate;
            }

            Object result = replacement.invoke(candidate.getValue(), context.getKey(), context.getDepth(), candidate, context);

            if (result instanceof CoreJsonNode) {
                return (CoreJsonNode) result;
            }

            return candidate.create(result);
        });
    }


    public static <T> CoreJsonNode<T> flatten(CoreJsonNode<T> node) {
        return flatten(node, -1);
    }

    public static <T> CoreJsonNode<T> flatten(CoreJsonNode<T> node, Number maxDepth) {
        if (node == null) {
            return null;
        }

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
            protected CoreJsonNode<T> visitArray(CoreJsonNodeVisitorContext<T> context, CoreJsonNode<T> node) {
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
            protected CoreJsonNode<T> visitChildren(CoreJsonNodeVisitorContext<T> context, CoreJsonNode<T> node) {
                return node;
            }
        }.visit(node);

        return node.createArray(childNodes);
    }

}

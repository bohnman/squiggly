package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNode;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeContext;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeType;
import com.github.bohnman.squiggly.json.node.support.BaseJsonNodeVisitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Functions that work with json node objects.
 */
public class SquigglyJsonNodeFunctions {

    private SquigglyJsonNodeFunctions() {
    }

    /**
     * Find all the nodes matching the predicate function.
     *
     * @param node   json node
     * @param lambda predicate node
     * @param <T>    type
     * @return matches
     */
    public static <T> SquigglyJsonNode<T> findAll(SquigglyJsonNode<T> node, CoreLambda lambda) {
        if (node == null) {
            return null;
        }

        if (lambda == null) {
            return node;
        }

        List<SquigglyJsonNode<T>> matches = new ArrayList<>();

        new BaseJsonNodeVisitor<T>() {
            @Override
            protected SquigglyJsonNode<T> visitAtomNode(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
                Map.Entry entry = CorePair.of(context.getKey(), node.getValue());

                boolean result = CoreConversions.toBoolean(lambda.invoke(entry, context.getDepth(), node, context));

                if (result) {
                    matches.add(node);
                }

                return node;
            }
        }.visit(node);

        return node.createArray(matches);
    }

    /**
     * Convert matching json nodes.
     *
     * @param node             the base node
     * @param predicate        predicate function
     * @param valueReplacement mapping function
     * @param <T>              type
     * @return result
     */
    public static <T> SquigglyJsonNode transform(SquigglyJsonNode<T> node, CoreLambda predicate, CoreLambda valueReplacement) {
        return transform(node, predicate, valueReplacement, CoreLambda.identity());
    }

    /**
     * Convert matching json nodes.
     *
     * @param node             the base node
     * @param predicate        predicate function
     * @param valueReplacement mapping function
     * @param keyReplacement   mapping function for keys if it is a hash
     * @param <T>              type
     * @return result
     */
    @SuppressWarnings("unchecked")
    public static <T> SquigglyJsonNode transform(SquigglyJsonNode<T> node, CoreLambda predicate, CoreLambda valueReplacement, CoreLambda keyReplacement) {
        if (node == null) {
            return null;
        }

        if (predicate == null || valueReplacement == null || keyReplacement == null) {
            return node;
        }

        return node.transform((context, candidate) -> {
            Map.Entry entry = CorePair.of(context.getKey(), candidate.getValue());
            boolean test = CoreConversions.toBoolean(predicate.invoke(entry, context.getDepth(), candidate, context));

            if (!test) {
                return candidate;
            }

            context.setKey(keyReplacement.invoke(context.getKey(), candidate.getValue(), context.getDepth(), candidate, context));

            Object result = valueReplacement.invoke(candidate.getValue(), context.getKey(), context.getDepth(), candidate, context);

            if (result instanceof SquigglyJsonNode) {
                return (SquigglyJsonNode) result;
            }

            return candidate.create(result);
        });
    }

    /**
     * Recursively flatten array nodes into a single array.
     *
     * @param node base node
     * @param <T>  type
     * @return flattened nodes
     */
    public static <T> SquigglyJsonNode<T> flatten(SquigglyJsonNode<T> node) {
        return flatten(node, -1);
    }

    /**
     * Recursively flatten array nodes into a single array, up to maxDepth.
     *
     * @param node base node
     * @param <T>  type
     * @return flattened nodes
     */
    public static <T> SquigglyJsonNode<T> flatten(SquigglyJsonNode<T> node, Number maxDepth) {
        if (node == null) {
            return null;
        }

        if (node.getType() != SquigglyJsonNodeType.ARRAY) {
            return node;
        }

        maxDepth = CoreObjects.firstNonNull(maxDepth, -1);
        int maxDepthInt = maxDepth.intValue();

        if (maxDepthInt == 0) {
            return node;
        }

        List<SquigglyJsonNode<T>> childNodes = new ArrayList<>();

        new BaseJsonNodeVisitor<T>() {

            @Override
            protected SquigglyJsonNode<T> visitArray(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
                for (SquigglyJsonNode<T> child : node.getArrayElements()) {
                    if (child.isArray() && (maxDepthInt < 0 || ((context.getDepth() + 1) <= maxDepthInt))) {
                        visitArray(context.descend(null, node, null, null), child);
                    } else {
                        childNodes.add(child);
                    }
                }

                return node;
            }

            @Override
            protected SquigglyJsonNode<T> visitChildren(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
                return node;
            }
        }.visit(node);

        return node.createArray(childNodes);
    }

}

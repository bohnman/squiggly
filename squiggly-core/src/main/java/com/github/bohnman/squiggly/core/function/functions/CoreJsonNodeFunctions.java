package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.json.node.BaseCoreJsonNodeVisitor;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNodeType;
import com.github.bohnman.core.json.node.CoreJsonNodeVisitorContext;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.tuple.CorePair;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Functions that work with json node objects.
 */
public class CoreJsonNodeFunctions {

    private CoreJsonNodeFunctions() {
    }

    /**
     * Find all the nodes matching the predicate function.
     *
     * @param node   json node
     * @param lambda predicate node
     * @param <T>    type
     * @return matches
     */
    public static <T> CoreJsonNode<T> findAll(CoreJsonNode<T> node, CoreLambda lambda) {
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
    public static <T> CoreJsonNode transform(CoreJsonNode<T> node, CoreLambda predicate, CoreLambda valueReplacement) {
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
    public static <T> CoreJsonNode transform(CoreJsonNode<T> node, CoreLambda predicate, CoreLambda valueReplacement, CoreLambda keyReplacement) {
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

            if (result instanceof CoreJsonNode) {
                return (CoreJsonNode) result;
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
    public static <T> CoreJsonNode<T> flatten(CoreJsonNode<T> node) {
        return flatten(node, -1);
    }

    /**
     * Recursively flatten array nodes into a single array, up to maxDepth.
     *
     * @param node base node
     * @param <T>  type
     * @return flattened nodes
     */
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

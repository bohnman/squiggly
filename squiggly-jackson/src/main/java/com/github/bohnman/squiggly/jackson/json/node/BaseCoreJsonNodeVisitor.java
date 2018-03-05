package com.github.bohnman.squiggly.jackson.json.node;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.jackson.json.path.CoreJsonPathElement;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class BaseCoreJsonNodeVisitor<T> {
    public CoreJsonNode<T> visit(CoreJsonNode<T> node) {
        CoreJsonNode<T> result = visitGenericNode(new CoreJsonNodeVisitorContext(), CoreAssert.notNull(node));
        return (result == null) ? node.create(null) : result;
    }

    protected CoreJsonNode<T> visitGenericNode(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        switch (node.getType()) {
            case ARRAY:
                return visitArray(context, node);
            case BINARY:
                return visitBinary(context, node);
            case BOOLEAN:
                return visitBoolean(context, node);
            case NULL:
                return visitNull(context, node);
            case NUMBER:
                return visitNumber(context, node);
            case STRING:
                return visitString(context, node);
            case OBJECT:
                return visitObject(context, node);
            default:
                throw new IllegalStateException("Unhandled node type: " + node.getType());
        }
    }

    protected CoreJsonNode<T> visitArray(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        node = visitAtomNode(context, node);

        if (node == null) {
            return null;
        }

        if (node.getType() != CoreJsonNodeType.ARRAY) {
            return visitGenericNode(context, node);
        }

        List<CoreJsonNode<T>> newElements = null;

        for (int i = 0; i < node.getArrayElements().size(); i++) {
            CoreJsonNode<T> child = node.getArrayElements().get(i);
            CoreJsonNode<T> newChild = visitGenericNode(context.descend(i), child);

            if (child != newChild) {
                if (newElements == null) newElements = new ArrayList<>(node.getArrayElements());
                newElements.set(i, newChild);
            }
        }

        if (newElements != null) {
            newElements.removeIf(Objects::isNull);
            return node.createArray(newElements);
        }

        return node;
    }

    protected CoreJsonNode<T> visitBinary(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected CoreJsonNode<T> visitBoolean(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected CoreJsonNode<T> visitNull(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected CoreJsonNode<T> visitNumber(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected CoreJsonNode<T> visitString(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected CoreJsonNode<T> visitObject(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        node = visitAtomNode(context, node);

        if (node == null) {
            return null;
        }

        if (node.getType() != CoreJsonNodeType.OBJECT) {
            return visitGenericNode(context, node);
        }

        List<CorePair<String, CoreJsonNode<T>>> newElements = null;
        Object object = node.getValue();

        for (int i = 0; i < node.getObjectElements().size(); i++) {
            CorePair<String, CoreJsonNode<T>> pair = node.getObjectElements().get(i);
            CoreJsonNode<T> child = pair.getRight();
            String key = pair.getLeft();
            CoreJsonNodeVisitorContext newContext = context.descend(key, new CoreJsonPathElement(key, object));
            CoreJsonNode<T> newChild = visitGenericNode(newContext, pair.getRight());
            key = CoreConversions.toString(newContext.getKey());

            if (child != newChild || !Objects.equals(pair.getLeft(), key)) {
                if (newElements == null) newElements = new ArrayList<>(node.getObjectElements());
                if (key == null || newChild == null) {
                    newElements.set(i, null);
                } else {
                    newElements.set(i, CorePair.of(key, newChild));
                }
            }
        }

        if (newElements != null) {
            newElements.removeIf(Objects::isNull);
            return node.createObject(newElements);
        }

        return node;
    }


    protected CoreJsonNode<T> visitAtomNode(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
        return node;
    }

}

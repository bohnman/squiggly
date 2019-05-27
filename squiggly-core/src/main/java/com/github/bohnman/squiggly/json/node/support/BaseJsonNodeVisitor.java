package com.github.bohnman.squiggly.json.node.support;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNode;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeContext;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeType;
import com.github.bohnman.squiggly.path.SquigglyObjectPathElement;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class BaseJsonNodeVisitor<T> {
    public SquigglyJsonNode<T> visit(SquigglyJsonNode<T> node) {
        SquigglyJsonNode<T> result = visitGenericNode(new SquigglyJsonNodeContext<>(), CoreAssert.notNull(node));
        return (result == null) ? node.create(null) : result;
    }

    protected SquigglyJsonNode<T> visitGenericNode(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        SquigglyJsonNode<T> result;

        switch (node.getType()) {
            case ARRAY:
                result = visitArray(context, node);
                break;
            case BINARY:
                result = visitBinary(context, node);
                break;
            case BOOLEAN:
                result = visitBoolean(context, node);
                break;
            case NULL:
                result = visitNull(context, node);
                break;
            case NUMBER:
                result = visitNumber(context, node);
                break;
            case STRING:
                result = visitString(context, node);
                break;
            case OBJECT:
                result = visitObject(context, node);
                break;
            default:
                throw new IllegalStateException("Unhandled node type: " + node.getType());
        }

        if (result == null) {
            return null;
        }

        return visitChildren(context, result);
    }

    protected SquigglyJsonNode<T> visitChildren(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        if (node.getType() == SquigglyJsonNodeType.ARRAY) {
            return visitArrayChildren(context, node);
        }

        if (node.getType() == SquigglyJsonNodeType.OBJECT) {
            return visitObjectChildren(context, node);
        }

        return node;
    }

    protected SquigglyJsonNode<T> visitArray(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitArrayChildren(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        List<SquigglyJsonNode<T>> newElements = null;
        Object array = node.getValue();

        for (int i = 0; i < node.getArrayElements().size(); i++) {
            SquigglyJsonNode<T> child = node.getArrayElements().get(i);
            SquigglyJsonNode<T> newChild = visitGenericNode(context.descend(i, node, new SquigglyObjectPathElement(Integer.toString(i), array, child.getValue()), null), child);

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

    protected SquigglyJsonNode<T> visitBinary(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitBoolean(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitNull(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitNumber(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitString(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitObject(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return visitAtomNode(context, node);
    }

    protected SquigglyJsonNode<T> visitObjectChildren(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        List<CorePair<String, SquigglyJsonNode<T>>> newElements = null;
        Object object = node.getValue();

        for (int i = 0; i < node.getObjectElements().size(); i++) {
            CorePair<String, SquigglyJsonNode<T>> pair = node.getObjectElements().get(i);
            SquigglyJsonNode<T> child = pair.getRight();
            String key = pair.getLeft();
            SquigglyObjectPathElement objectPathElement = new SquigglyObjectPathElement(key, object, child.getValue());
            SquigglyJsonNodeContext<T> newContext = context.descend(key, node, objectPathElement, objectPathElement);
            SquigglyJsonNode<T> newChild = visitGenericNode(newContext, pair.getRight());
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


    protected SquigglyJsonNode<T> visitAtomNode(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
        return node;
    }

}

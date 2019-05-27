package com.github.bohnman.squiggly.json.node;

import com.github.bohnman.core.tuple.CorePair;

import java.util.List;

public interface SquigglyJsonNode<T> {

    SquigglyJsonNodeType getType();

    Object getValue();

    T getRawNode();

    List<CorePair<String, SquigglyJsonNode<T>>> getObjectElements();

    SquigglyJsonNode<T> create(Object value);

    SquigglyJsonNode<T> createArray(List<SquigglyJsonNode<T>> elements);

    SquigglyJsonNode<T> createObject(List<CorePair<String, SquigglyJsonNode<T>>> elements);

    List<SquigglyJsonNode<T>> getArrayElements();

    List<SquigglyJsonNode<T>> find(SquigglyJsonNodePredicate<T> predicate);

    SquigglyJsonNode<T> transform(SquigglyJsonNodeFunction<T> function);

    default boolean isType(SquigglyJsonNodeType type) {
        return getType() == type;
    }

    default boolean isArray() {
        return isType(SquigglyJsonNodeType.ARRAY);
    }

    default boolean isBinary() {
        return isType(SquigglyJsonNodeType.BINARY);
    }

    default boolean isBoolean() {
        return isType(SquigglyJsonNodeType.BOOLEAN);
    }

    default boolean isNull() {
        return isType(SquigglyJsonNodeType.NULL);
    }

    default boolean isNumber() {
        return isType(SquigglyJsonNodeType.NUMBER);
    }

    default boolean isObject() {
        return isType(SquigglyJsonNodeType.OBJECT);
    }

    default boolean isString() {
        return isType(SquigglyJsonNodeType.STRING);
    }
}

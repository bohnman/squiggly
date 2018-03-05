package com.github.bohnman.squiggly.jackson.json.node;

import com.github.bohnman.core.tuple.CorePair;

import java.util.List;

public interface CoreJsonNode<T> {

    CoreJsonNodeType getType();

    Object getValue();

    T getRawNode();

    List<CorePair<String, CoreJsonNode<T>>> getObjectElements();

    CoreJsonNode<T> create(Object value);

    CoreJsonNode<T> createArray(List<CoreJsonNode<T>> elements);

    CoreJsonNode<T> createObject(List<CorePair<String, CoreJsonNode<T>>> elements);

    List<CoreJsonNode<T>> getArrayElements();

    List<CoreJsonNode<T>> find(CoreJsonNodePredicate<T> predicate);

    CoreJsonNode<T> transform(CoreJsonNodeFunction<T> function);
}

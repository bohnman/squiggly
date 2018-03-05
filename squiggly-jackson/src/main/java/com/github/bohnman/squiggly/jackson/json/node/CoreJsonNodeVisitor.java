package com.github.bohnman.squiggly.jackson.json.node;

public interface CoreJsonNodeVisitor<T, R> {

    R visit(CoreJsonNode<T> node);
}

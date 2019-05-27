package com.github.bohnman.squiggly.json.node;

public interface SquigglyJsonNodeVisitor<T, R> {

    R visit(SquigglyJsonNode<T> node);
}

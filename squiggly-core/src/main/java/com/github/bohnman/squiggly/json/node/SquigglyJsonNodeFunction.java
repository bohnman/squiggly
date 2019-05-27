package com.github.bohnman.squiggly.json.node;

public interface SquigglyJsonNodeFunction<T> {

    SquigglyJsonNode<T> apply(SquigglyJsonNodeContext context, SquigglyJsonNode<T> node);
}

package com.github.bohnman.squiggly.json.node;

@FunctionalInterface
public interface SquigglyJsonNodePredicate<T> {

    boolean test(SquigglyJsonNodeContext context, SquigglyJsonNode<T> node);
}

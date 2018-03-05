package com.github.bohnman.squiggly.jackson.json.node;

@FunctionalInterface
public interface CoreJsonNodePredicate<T> {

    boolean test(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node);
}

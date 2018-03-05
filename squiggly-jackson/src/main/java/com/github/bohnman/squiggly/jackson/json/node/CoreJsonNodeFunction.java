package com.github.bohnman.squiggly.jackson.json.node;

public interface CoreJsonNodeFunction<T> {

    CoreJsonNode<T> apply(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node);
}

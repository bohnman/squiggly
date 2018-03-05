package com.github.bohnman.squiggly.jackson.json.node;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public abstract class BaseCoreJsonNode<T> implements CoreJsonNode<T> {

    @Override
    public List<CoreJsonNode<T>> find(CoreJsonNodePredicate<T> predicate) {
        List<CoreJsonNode<T>> nodes = new ArrayList<>();

        new BaseCoreJsonNodeVisitor<T>() {
            @Override
            protected CoreJsonNode<T> visitAtomNode(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
                if (predicate.test(context, node)) {
                    nodes.add(node);
                }

                return super.visitAtomNode(context, node);
            }
        }.visit(this);

        return Collections.unmodifiableList(nodes);
    }

    @Override
    public CoreJsonNode<T> transform(CoreJsonNodeFunction<T> function) {
        return new BaseCoreJsonNodeVisitor<T>() {
            @Override
            protected CoreJsonNode<T> visitAtomNode(CoreJsonNodeVisitorContext context, CoreJsonNode<T> node) {
                return function.apply(context, node);
            }
        }.visit(this);
    }
}

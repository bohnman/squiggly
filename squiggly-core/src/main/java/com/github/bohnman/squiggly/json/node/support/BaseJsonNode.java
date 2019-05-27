package com.github.bohnman.squiggly.json.node.support;

import com.github.bohnman.squiggly.json.node.SquigglyJsonNode;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeFunction;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodePredicate;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNodeContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public abstract class BaseJsonNode<T> implements SquigglyJsonNode<T> {

    @Override
    public List<SquigglyJsonNode<T>> find(SquigglyJsonNodePredicate<T> predicate) {
        List<SquigglyJsonNode<T>> nodes = new ArrayList<>();

        new BaseJsonNodeVisitor<T>() {
            @Override
            protected SquigglyJsonNode<T> visitAtomNode(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
                if (predicate.test(context, node)) {
                    nodes.add(node);
                }

                return super.visitAtomNode(context, node);
            }
        }.visit(this);

        return Collections.unmodifiableList(nodes);
    }

    @Override
    public SquigglyJsonNode<T> transform(SquigglyJsonNodeFunction<T> function) {
        return new BaseJsonNodeVisitor<T>() {
            @Override
            protected SquigglyJsonNode<T> visitAtomNode(SquigglyJsonNodeContext<T> context, SquigglyJsonNode<T> node) {
                return function.apply(context, node);
            }
        }.visit(this);
    }
}

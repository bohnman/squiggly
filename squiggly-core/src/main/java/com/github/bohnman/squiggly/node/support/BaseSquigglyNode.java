package com.github.bohnman.squiggly.node.support;

import com.github.bohnman.squiggly.parse.SquigglyParseContext;
import com.github.bohnman.squiggly.node.SquigglyNode;

public abstract class BaseSquigglyNode implements SquigglyNode {

    private final SquigglyParseContext context;

    public BaseSquigglyNode(SquigglyParseContext context) {
        this.context = context;
    }

    @Override
    public SquigglyParseContext getContext() {
        return context;
    }

}

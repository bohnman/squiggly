package com.github.bohnman.squiggly.core.parse.nodes;

import com.github.bohnman.squiggly.core.parse.ParseContext;
import com.github.bohnman.squiggly.core.parse.SquigglyNode;

public abstract class BaseSquigglyNode implements SquigglyNode {

    private final ParseContext context;

    public BaseSquigglyNode(ParseContext context) {
        this.context = context;
    }

    @Override
    public ParseContext getContext() {
        return context;
    }

}

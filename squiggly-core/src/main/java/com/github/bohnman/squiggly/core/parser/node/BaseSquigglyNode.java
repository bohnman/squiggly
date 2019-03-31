package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

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

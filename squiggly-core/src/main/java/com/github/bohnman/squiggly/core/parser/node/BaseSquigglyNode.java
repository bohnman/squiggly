package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

public abstract class BaseSquigglyNode implements SquigglyNode {

    private final ParseContext context;
    private final SquigglyNodeType type;

    public BaseSquigglyNode(ParseContext context, SquigglyNodeType type) {
        this.context = context;
        this.type = type;
    }

    @Override
    public ParseContext getContext() {
        return context;
    }

    @Override
    public SquigglyNodeType getType() {
        return type;
    }
}

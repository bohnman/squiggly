package com.github.bohnman.squiggly.parse.support;

import com.github.bohnman.squiggly.parse.ParseContext;
import com.github.bohnman.squiggly.parse.SquigglyNode;

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

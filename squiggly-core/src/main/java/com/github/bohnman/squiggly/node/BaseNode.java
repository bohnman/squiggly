package com.github.bohnman.squiggly.node;

public abstract class BaseNode implements SquigglyNode {

    private final SquigglyNodeOrigin origin;

    public BaseNode(SquigglyNodeOrigin origin) {
        this.origin = origin;
    }

    @Override
    public SquigglyNodeOrigin getOrigin() {
        return origin;
    }

}

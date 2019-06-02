package com.github.bohnman.squiggly.node;

public abstract class BaseNodeBuilder<T extends SquigglyNode> {

    private final SquigglyNodeOrigin origin;

    protected BaseNodeBuilder(SquigglyNodeOrigin origin) {
        this.origin = origin;
    }

    public SquigglyNodeOrigin getOrigin() {
        return origin;
    }

    public abstract T build();
}

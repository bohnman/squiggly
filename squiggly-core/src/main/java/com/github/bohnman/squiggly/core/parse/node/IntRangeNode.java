package com.github.bohnman.squiggly.core.parse.node;

public class IntRangeNode {

    private final ArgumentNode start;
    private final ArgumentNode end;
    private final boolean exclusiveEnd;

    public IntRangeNode(ArgumentNode.Builder start, ArgumentNode.Builder end, boolean exclusiveEnd) {
        this.start = start == null ? null : start.index(0).build();
        this.end = end == null ? null : end.index(1).build();
        this.exclusiveEnd = exclusiveEnd;
    }

    public ArgumentNode getStart() {
        return start;
    }

    public ArgumentNode getEnd() {
        return end;
    }

    public boolean isExclusiveEnd() {
        return exclusiveEnd;
    }
}

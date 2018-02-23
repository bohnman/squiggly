package com.github.bohnman.squiggly.parser;

public class IntRangeNode {

    private final ArgumentNode start;
    private final ArgumentNode end;

    public IntRangeNode(ArgumentNode.Builder start, ArgumentNode.Builder end) {
        this.start = start == null ? null : start.index(0).build();
        this.end = end == null ? null : end.index(1).build();
    }

    public ArgumentNode getStart() {
        return start;
    }

    public ArgumentNode getEnd() {
        return end;
    }
}

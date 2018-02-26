package com.github.bohnman.core.range;

public class CoreIntRange {

    private final Integer start;
    private final Integer end;

    public CoreIntRange(Integer start, Integer end) {
        this.start = start;
        this.end = end;
    }

    public Integer getStart() {
        return start;
    }

    public Integer getEnd() {
        return end;
    }

    @Override
    public String toString() {
        String startString = start == null ? "" : Integer.toString(start);
        String endString = end == null ? "" : Integer.toString(end);
        return "[" + startString + ':' + endString + ']';
    }
}

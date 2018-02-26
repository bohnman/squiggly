package com.github.bohnman.core.range;

public class CoreIntRange {

    private final Integer start;
    private final Integer end;
    private final boolean exclusive;

    public CoreIntRange(Integer start, Integer end, boolean exclusive) {
        this.start = start;
        this.end = end;
        this.exclusive = exclusive;
    }

    public Integer getStart() {
        return start;
    }

    public Integer getEnd() {
        return end;
    }

    public boolean isInclusive() {
        return !exclusive;
    }

    public boolean isExclusive() {
        return exclusive;
    }

    public CoreIntRange toExclusive() {
        if (isExclusive()) {
            return this;
        }

        Integer inclusiveStart = start;
        Integer exclusiveEnd;

        if (end == null) {
            exclusiveEnd = null;
        } else if (end == -1) {
            exclusiveEnd = null;
        } else {
            exclusiveEnd = end + 1;
        }

        return new CoreIntRange(inclusiveStart, exclusiveEnd, true);
    }

    public CoreIntRange toInclusive() {
        if (isInclusive()) {
            return this;
        }

        Integer inclusiveStart = start;
        Integer inclusiveEnd;

        if (end == null) {
            inclusiveEnd = end;
        } else if (start != null && start.equals(end)) {
            inclusiveEnd = null;
        } else {
            inclusiveEnd = end - 1;
        }

        return new CoreIntRange(inclusiveStart, inclusiveEnd, false);

    }

    @Override
    public String toString() {
        String startString = start == null ? "" : Integer.toString(start);
        String endString = end == null ? "" : Integer.toString(end);
        return "[" + startString + ':' + endString + ']';
    }
}

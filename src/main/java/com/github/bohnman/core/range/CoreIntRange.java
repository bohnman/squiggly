package com.github.bohnman.core.range;

import com.github.bohnman.core.lang.array.CoreArrays;

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

    public boolean isEmpty() {
        if (start == null) {
            return true;
        }

        return exclusive && end != null && start.equals(end);
    }

    public CoreIntRange normalize(int len) {
        if (len <= 0) {
            return new CoreIntRange(null, null, exclusive);
        }

        if (start == null) {
            return new CoreIntRange(null, null, exclusive);
        }

        int normalizedStart = CoreArrays.normalizeIndex(start, len);
        int normalizedEnd =  end == null ? len : CoreArrays.normalizeIndex(end, len);

        if (normalizedEnd < normalizedStart) {
            return new CoreIntRange(null, null, exclusive);
        }

        if (exclusive && normalizedStart == normalizedEnd) {
            return new CoreIntRange(null, null, exclusive);
        }

        return new CoreIntRange(normalizedStart, normalizedEnd, exclusive);

    }

    @Override
    public String toString() {
        String startString = start == null ? "" : Integer.toString(start);
        String endString = end == null ? "" : Integer.toString(end);
        return "[" + startString + ':' + endString + ']';
    }
}

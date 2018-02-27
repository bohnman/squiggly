package com.github.bohnman.core.range;

import com.github.bohnman.core.lang.array.CoreArrays;

import javax.annotation.Nullable;

public class CoreIntRange {

    public static final CoreIntRange EMPTY_EXCLUSIVE = new CoreIntRange(null, null, false) {
        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public CoreIntRange toInclusive() {
            return EMPTY_INCLUSIVE;
        }
    };

    public static final CoreIntRange EMPTY_INCLUSIVE = new CoreIntRange(null, null, false) {
        @Override
        public boolean isEmpty() {
            return super.isEmpty();
        }

        @Override
        public CoreIntRange toExclusive() {
            return EMPTY_EXCLUSIVE;
        }
    };

    private final Integer start;
    private final Integer end;
    private final boolean exclusive;

    private CoreIntRange(@Nullable Integer start, Integer end, boolean exclusive) {
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

        if (start == null) {
            return EMPTY_EXCLUSIVE;
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

        if (start == null) {
            return EMPTY_INCLUSIVE;
        }

        Integer inclusiveStart = start;
        Integer inclusiveEnd;

        if (end == null) {
            inclusiveEnd = end;
        } else if (start.equals(end)) {
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
            return empty(exclusive);
        }

        if (start == null) {
            return empty(exclusive);
        }

        int normalizedStart = CoreArrays.normalizeIndex(start, len);
        int normalizedEnd =  end == null ? len : CoreArrays.normalizeIndex(end, len);

        if (normalizedEnd < normalizedStart) {
            return empty(exclusive);
        }

        if (exclusive && normalizedStart == normalizedEnd) {
            return empty(exclusive);
        }

        return new CoreIntRange(normalizedStart, normalizedEnd, exclusive);

    }

    @Override
    public String toString() {
        String startString = start == null ? "" : Integer.toString(start);
        String endString = end == null ? "" : Integer.toString(end);
        return "[" + startString + ':' + endString + (exclusive ? ')' : ']');
    }

    public static CoreIntRange inclusiveInclusive(int start) {
        return new CoreIntRange(start, -1, false);
    }

    public static CoreIntRange inclusiveInclusive(int start, int end) {
        return new CoreIntRange(start, -1, false);
    }

    public static CoreIntRange inclusiveExclusive(int start) {
        return new CoreIntRange(start, null, true);
    }

    public static CoreIntRange inclusiveExclusive(int start, int end) {
        return new CoreIntRange(start, end, true);
    }

    public static CoreIntRange emptyInclusive() {
        return EMPTY_INCLUSIVE;

    }

    public static CoreIntRange emptyExclusive() {
        return EMPTY_EXCLUSIVE;
    }

    private static CoreIntRange empty(boolean exclusive) {
        return exclusive ? EMPTY_EXCLUSIVE : EMPTY_INCLUSIVE;
    }
}

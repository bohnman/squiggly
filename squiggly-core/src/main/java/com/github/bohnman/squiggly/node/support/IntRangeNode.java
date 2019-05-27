package com.github.bohnman.squiggly.node.support;

import com.github.bohnman.squiggly.parse.SquigglyParseContext;

/**
 * Represents an integer range.
 */
public class IntRangeNode extends BaseSquigglyNode {

    private final ArgumentNode start;
    private final ArgumentNode end;
    private final boolean exclusiveEnd;

    /**
     * Constructor.
     *
     * @param start        start number
     * @param end          end number
     * @param exclusiveEnd whether the end is exclusive or inclusive
     */
    public IntRangeNode(SquigglyParseContext context, ArgumentNode.Builder start, ArgumentNode.Builder end, boolean exclusiveEnd) {
        super(context);
        this.start = start == null ? null : start.index(0).build();
        this.end = end == null ? null : end.index(1).build();
        this.exclusiveEnd = exclusiveEnd;
    }

    /**
     * Get the start number.
     *
     * @return start
     */
    public ArgumentNode getStart() {
        return start;
    }

    /**
     * Get the end number.
     *
     * @return end
     */
    public ArgumentNode getEnd() {
        return end;
    }

    /**
     * Indicate whether the end is exclusive or inclusive.
     *
     * @return true if exclusive
     */
    public boolean isExclusiveEnd() {
        return exclusiveEnd;
    }
}

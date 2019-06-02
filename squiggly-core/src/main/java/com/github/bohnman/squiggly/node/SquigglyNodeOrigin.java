package com.github.bohnman.squiggly.node;

/**
 * Holds positional information when parsing nodes.
 */
public class SquigglyNodeOrigin {

    private static final SquigglyNodeOrigin START = new SquigglyNodeOrigin(1, 1);

    private final int line;
    private final int column;

    /**
     * Construct with given position.
     *
     * @param line   line number
     * @param column column number
     */
    private SquigglyNodeOrigin(int line, int column) {
        this.line = line;
        this.column = column;
    }

    /**
     * Get line number.
     *
     * @return line
     */
    public int getLine() {
        return line;
    }

    /**
     * Get the column number.
     *
     * @return column
     */
    public int getColumn() {
        return column;
    }

    @Override
    public String toString() {
        return "[" + line + ':' + column + ']';
    }

    public static SquigglyNodeOrigin create() {
        return START;
    }

    public static SquigglyNodeOrigin create(int line, int column) {
        return new SquigglyNodeOrigin(line, column);
    }
}

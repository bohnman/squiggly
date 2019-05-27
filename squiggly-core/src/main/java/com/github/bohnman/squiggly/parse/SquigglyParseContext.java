package com.github.bohnman.squiggly.parse;

/**
 * Holds positional information when parsing nodes.
 */
public class SquigglyParseContext {

    public static final SquigglyParseContext EMPTY = new SquigglyParseContext();

    private final int line;
    private final int column;

    /**
     * Construct with position 1:1.
     */
    public SquigglyParseContext() {
        this(1, 1);
    }

    /**
     * Construct with given position.
     *
     * @param line   line number
     * @param column column number
     */
    public SquigglyParseContext(int line, int column) {
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
}

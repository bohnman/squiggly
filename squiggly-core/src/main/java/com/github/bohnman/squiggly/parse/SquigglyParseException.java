package com.github.bohnman.squiggly.parse;

import com.github.bohnman.squiggly.node.SquigglyNodeOrigin;

/**
 * Exception thrown when there is some sort of parsing error.
 */
public class SquigglyParseException extends RuntimeException {

    private final SquigglyNodeOrigin origin;

    /**
     * Constructor.
     *
     * @param origin parse context
     * @param message error message
     * @param vars    error arguments
     */
    public SquigglyParseException(SquigglyNodeOrigin origin, String message, Object... vars) {
        super(format(origin, message, vars));
        this.origin = origin;
    }

    /**
     * Constructor.
     *
     * @param origin parse context
     * @param message error message
     * @param cause   throwable cause
     * @param vars    error arguments
     */
    public SquigglyParseException(SquigglyNodeOrigin origin, String message, Throwable cause, Object... vars) {
        super(String.format(message, vars), cause);
        this.origin = origin;
    }

    /**
     * Gets the parse context.
     *
     * @return parse context
     */
    public SquigglyNodeOrigin getOrigin() {
        return origin;
    }

    private static String format(SquigglyNodeOrigin origin, String message, Object[] vars) {
        String contextStr = String.format("[%s:%s]: ", origin.getColumn(), origin.getLine());
        return contextStr + String.format(message, vars);
    }
}

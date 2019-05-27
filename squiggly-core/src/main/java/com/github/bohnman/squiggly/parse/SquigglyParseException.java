package com.github.bohnman.squiggly.parse;

/**
 * Exception thrown when there is some sort of parsing error.
 */
public class SquigglyParseException extends RuntimeException {

    private final SquigglyParseContext context;

    /**
     * Constructor.
     *
     * @param context parse context
     * @param message error message
     * @param vars    error arguments
     */
    public SquigglyParseException(SquigglyParseContext context, String message, Object... vars) {
        super(format(context, message, vars));
        this.context = context;
    }

    /**
     * Constructor.
     *
     * @param context parse context
     * @param message error message
     * @param cause   throwable cause
     * @param vars    error arguments
     */
    public SquigglyParseException(SquigglyParseContext context, String message, Throwable cause, Object... vars) {
        super(String.format(message, vars), cause);
        this.context = context;
    }

    /**
     * Gets the parse context.
     *
     * @return parse context
     */
    public SquigglyParseContext getContext() {
        return context;
    }

    private static String format(SquigglyParseContext context, String message, Object[] vars) {
        String contextStr = String.format("[%s:%s]: ", context.getColumn(), context.getLine());
        return contextStr + String.format(message, vars);
    }
}

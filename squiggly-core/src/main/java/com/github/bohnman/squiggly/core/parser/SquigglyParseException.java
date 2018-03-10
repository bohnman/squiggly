package com.github.bohnman.squiggly.core.parser;

public class SquigglyParseException extends RuntimeException {

    private final ParseContext context;

    public SquigglyParseException(ParseContext context, String message, Object... vars) {
        super(format(context, message, vars));
        this.context = context;
    }

    public SquigglyParseException(ParseContext context, String message, Throwable cause, Object... vars) {
        super(String.format(message, vars), cause);
        this.context = context;
    }

    public ParseContext getContext() {
        return context;
    }

    private static String format(ParseContext context, String message, Object[] vars) {
        String contextStr = String.format("[%s:%s]: ", context.getColumn(), context.getLine());
        return contextStr + String.format(message, vars);
    }
}

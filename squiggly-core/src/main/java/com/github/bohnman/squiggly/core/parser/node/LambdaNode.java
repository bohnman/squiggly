package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

import java.util.Collections;
import java.util.List;

/**
 * Represents a lambda call.
 */
public class LambdaNode {

    private final ParseContext context;
    private final List<String> arguments;
    private final FunctionNode body;

    /**
     * Constructor.
     *
     * @param context   parse context
     * @param arguments lambda arguments
     * @param body      execution logic
     */
    public LambdaNode(ParseContext context, List<String> arguments, FunctionNode body) {
        this.context = context;
        this.arguments = Collections.unmodifiableList(arguments);
        this.body = body;
    }

    /**
     * Gets the parse context.
     *
     * @return parse context
     */
    public ParseContext getContext() {
        return context;
    }

    /**
     * Gets the arguments.
     *
     * @return args
     */
    public List<String> getArguments() {
        return arguments;
    }

    /**
     * Gets the lambda body.
     *
     * @return body
     */
    public FunctionNode getBody() {
        return body;
    }
}

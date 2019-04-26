package com.github.bohnman.squiggly.parse.support;

import com.github.bohnman.squiggly.parse.ParseContext;

import java.util.Collections;
import java.util.List;

/**
 * Represents a lambda call.
 */
public class LambdaNode extends BaseSquigglyNode {

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
        super(context);
        this.arguments = Collections.unmodifiableList(arguments);
        this.body = body;
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

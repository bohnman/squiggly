package com.github.bohnman.squiggly.node;

import java.util.Collections;
import java.util.List;

/**
 * Represents a lambda call.
 */
public class LambdaNode extends BaseNode {

    private final List<String> arguments;
    private final FunctionNode body;

    /**
     * Constructor.
     *
     * @param origin   parse origin
     * @param body      execution logic
     * @param arguments lambda arguments
     */
    LambdaNode(SquigglyNodeOrigin origin, FunctionNode body, List<String> arguments) {
        super(origin);
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

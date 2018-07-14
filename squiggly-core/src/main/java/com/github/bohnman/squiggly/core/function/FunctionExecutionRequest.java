package com.github.bohnman.squiggly.core.function;

import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Holder object that is used as the input argument to execute a function.
 */
public class FunctionExecutionRequest {

    private final Object input;
    private final List<Object> parameters;

    /**
     * Constructor.
     *
     * @param input      the input object
     * @param parameters the function parameters
     */
    public FunctionExecutionRequest(Object input, List<Object> parameters) {
        this.input = input;
        this.parameters = notNull(parameters);
    }

    /**
     * The input object.
     *
     * @return input
     */
    public Object getInput() {
        return input;
    }

    /**
     * The function params.
     *
     * @return params
     */
    public List<Object> getParameters() {
        return parameters;
    }
}

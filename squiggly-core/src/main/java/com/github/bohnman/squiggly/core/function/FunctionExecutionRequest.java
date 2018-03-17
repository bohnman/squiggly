package com.github.bohnman.squiggly.core.function;

import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class FunctionExecutionRequest {

    private final Object input;
    private final List<Object> parameters;

    public FunctionExecutionRequest(Object input, List<Object> parameters) {
        this.input = input;
        this.parameters = notNull(parameters);
    }

    public Object getInput() {
        return input;
    }

    public List<Object> getParameters() {
        return parameters;
    }
}

package com.github.bohnman.squiggly.core.function;

import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class FunctionRequest {

    private final Object input;
    private final List<Object> parameters;

    public FunctionRequest(Object input, List<Object> parameters) {
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

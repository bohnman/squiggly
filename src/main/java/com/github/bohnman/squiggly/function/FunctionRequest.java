package com.github.bohnman.squiggly.function;

import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

public class FunctionRequest {

    private final Object input;
    private final List<Object> parameters;

    public FunctionRequest(Object input, List<Object> parameters) {
        this.input = input;
        this.parameters = checkNotNull(parameters);
    }

    public Object getInput() {
        return input;
    }

    public List<Object> getParameters() {
        return parameters;
    }
}

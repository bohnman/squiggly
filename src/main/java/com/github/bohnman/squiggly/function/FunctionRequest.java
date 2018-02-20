package com.github.bohnman.squiggly.function;

public class FunctionRequest {

    private final Object input;

    public FunctionRequest(Object input) {
        this.input = input;
    }

    public Object getInput() {
        return input;
    }
}

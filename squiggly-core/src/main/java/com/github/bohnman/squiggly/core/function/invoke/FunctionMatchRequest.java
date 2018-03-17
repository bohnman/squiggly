package com.github.bohnman.squiggly.core.function.invoke;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.parser.FunctionNode;

import java.util.List;

public class FunctionMatchRequest {

    private final FunctionNode functionNode;
    private final List<Object> parameters;
    private final Object input;
    private final List<SquigglyFunction<Object>> functions;

    public FunctionMatchRequest(FunctionNode functionNode, Object input, List<Object> parameters, List<SquigglyFunction<Object>> functions) {
        this.functionNode = functionNode;
        this.parameters = parameters;
        this.input = input;
        this.functions = functions;
    }

    public FunctionNode getFunctionNode() {
        return functionNode;
    }

    public List<SquigglyFunction<Object>> getFunctions() {
        return functions;
    }

    public List<Object> getParameters() {
        return parameters;
    }

    public Object getInput() {
        return input;
    }
}

package com.github.bohnman.squiggly.core.function.invoke;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.parser.node.FunctionNode;

import java.util.List;

/**
 * Holder object for all criteria use to match a function from a list of candidates.
 */
public class FunctionMatchRequest {

    private final FunctionNode functionNode;
    private final List<Object> parameters;
    private final Object input;
    private final List<SquigglyFunction<Object>> functions;
    private final Object childScope;
    private final Object parentScope;

    /**
     * Constructor.
     *
     * @param functionNode function node
     * @param input        the input object
     * @param childScope   child scope
     * @param parentScope  parent scope
     * @param parameters   the function parameters
     * @param functions    the candidates
     */
    public FunctionMatchRequest(FunctionNode functionNode, Object input, Object childScope, Object parentScope, List<Object> parameters, List<SquigglyFunction<Object>> functions) {
        this.functionNode = functionNode;
        this.parameters = parameters;
        this.input = input;
        this.functions = functions;
        this.childScope = childScope;
        this.parentScope = parentScope;
    }

    /**
     * Gets the function node.
     *
     * @return node
     */
    public FunctionNode getFunctionNode() {
        return functionNode;
    }

    /**
     * Gets the candidate functions.
     *
     * @return candidate functions
     */
    public List<SquigglyFunction<Object>> getFunctions() {
        return functions;
    }

    /**
     * Get the function parameters.
     *
     * @return function params
     */
    public List<Object> getParameters() {
        return parameters;
    }

    /**
     * Get the input object.
     *
     * @return input
     */
    public Object getInput() {
        return input;
    }

    public Object getChildScope() {
        return childScope;
    }

    public Object getParentScope() {
        return parentScope;
    }
}

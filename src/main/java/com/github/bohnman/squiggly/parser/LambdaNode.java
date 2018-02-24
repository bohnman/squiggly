package com.github.bohnman.squiggly.parser;

import com.google.common.collect.ImmutableList;

import java.util.List;

public class LambdaNode {

    private final ParseContext context;
    private final List<String> arguments;
    private final FunctionNode body;

    public LambdaNode(ParseContext context, List<String> arguments, FunctionNode body) {
        this.context = context;
        this.arguments = ImmutableList.copyOf(arguments);
        this.body = body;
    }

    public ParseContext getContext() {
        return context;
    }

    public List<String> getArguments() {
        return arguments;
    }

    public FunctionNode getBody() {
        return body;
    }
}

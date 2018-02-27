package com.github.bohnman.squiggly.core.parser;

import java.util.Collections;
import java.util.List;

public class LambdaNode {

    private final ParseContext context;
    private final List<String> arguments;
    private final FunctionNode body;

    public LambdaNode(ParseContext context, List<String> arguments, FunctionNode body) {
        this.context = context;
        this.arguments = Collections.unmodifiableList(arguments);
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

package com.github.bohnman.squiggly.core.config;

import com.github.bohnman.core.lang.CoreStrings;

@SuppressWarnings("unchecked")
public enum SystemFunctionName {

    ADD,
    AND,
    ASSIGN,
    DIVIDE,
    EQUALS,
    GET,
    GREATER_THAN,
    GREATER_THAN_EQUALS,
    IDENTITY,
    LESS_THAN,
    LESS_THAN_EQUALS,
    MATCH,
    MODULUS,
    MULTIPLY,
    NOT,
    NOT_EQUALS,
    NOT_MATCH,
    PROPERTY,
    OR,
    SLICE,
    SUBTRACT;

    private final String functionName;

    SystemFunctionName() {
        this.functionName = CoreStrings.camel(name());
    }

    public String getFunctionName() {
        return functionName;
    }
}
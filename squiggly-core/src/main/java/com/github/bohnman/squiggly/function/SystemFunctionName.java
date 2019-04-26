package com.github.bohnman.squiggly.function;

import com.github.bohnman.core.lang.CoreStrings;

/**
 * Built-in function catalog.
 */
@SuppressWarnings("unchecked")
public enum SystemFunctionName {

    ADD,
    AND,
    ASSIGN,
    DIVIDE,
    DEFAULT,
    EQUALS,
    GET,
    GREATER_THAN,
    GREATER_THAN_EQUALS,
    LESS_THAN,
    LESS_THAN_EQUALS,
    MATCH,
    MODULUS,
    MULTIPLY,
    NOT,
    NOT_EQUALS,
    NOT_MATCH,
    PICK,
    PROPERTY,
    OR,
    SELF,
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
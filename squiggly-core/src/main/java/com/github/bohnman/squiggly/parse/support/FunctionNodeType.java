package com.github.bohnman.squiggly.parse.support;

/**
 * Indicates the category of the function node.
 */
public enum FunctionNodeType {

    /**
     * A function that performs an assignment.  Eg. foo=bar
     */
    ASSIGNMENT,

    /**
     * A regular function.
     */
    FUNCTION,

    /**
     * A property retrieving function.
     */
    PROPERTY,
}

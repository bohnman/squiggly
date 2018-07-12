package com.github.bohnman.squiggly.core.parser.node;

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

    /**
     * A function that performs a self assignment.  Eg. foo.=bar
     */
    SELF_ASSIGNMENT
}

package com.github.bohnman.squiggly.core.name;

/**
 * Indicates match whose value is the result of variable resolution.
 */
public class VariableName extends ExactName {

    /**
     * Constructor.
     *
     * @param name name of the variable
     */
    public VariableName(String name) {
        super(name);
    }
}

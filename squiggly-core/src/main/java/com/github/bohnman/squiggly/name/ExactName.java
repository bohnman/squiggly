package com.github.bohnman.squiggly.name;

/**
 * Represents an exact name match.
 */
public class ExactName extends BaseSquigglyName {

    private final String name;

    /**
     * Constructor.
     *
     * @param name the extact name
     */
    public ExactName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getSpecificity() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean matches(String name) {
        return this.name.equals(name);
    }
}

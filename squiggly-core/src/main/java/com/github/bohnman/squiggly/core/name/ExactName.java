package com.github.bohnman.squiggly.core.name;

/**
 * Represents an exact name match.
 */
public class ExactName implements SquigglyName {

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
    public String getRawName() {
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

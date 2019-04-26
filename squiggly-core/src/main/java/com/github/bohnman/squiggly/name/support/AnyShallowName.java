package com.github.bohnman.squiggly.name.support;

/**
 * Represent a name that matches any node at the current nesting level.
 */
public class AnyShallowName extends BaseSquigglyName {

    public static final String ID = "*";
    private static final AnyShallowName INSTANCE = new AnyShallowName();

    @Override
    public String getName() {
        return ID;
    }

    @Override
    public int getSpecificity() {
        return 1;
    }

    @Override
    public boolean matches(String name) {
        return true;
    }

    /**
     * Get the singleton instance of the name.
     *
     * @return instance
     */
    public static AnyShallowName get() {
        return INSTANCE;
    }
}

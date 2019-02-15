package com.github.bohnman.squiggly.core.name;

/**
 * Represent a name that matches any node at the current nesting level.
 */
public class AnyShallowName implements SquigglyName {

    public static final String ID = "*";
    private static final AnyShallowName INSTANCE = new AnyShallowName();

    @Override
    public String getName() {
        return ID;
    }

    @Override
    public String getRawName() {
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

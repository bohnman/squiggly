package com.github.bohnman.squiggly.name;

/**
 * Indicates that the node name matches any node at any nesting level.
 */
public class NeverMatchName extends BaseSquigglyName {

    public static final String ID = "__NEVER__";

    private static final NeverMatchName INSTANCE = new NeverMatchName();

    @Override
    public String getName() {
        return ID;
    }

    @Override
    public int getSpecificity() {
        return 0;
    }

    @Override
    public boolean matches(String name) {
        return false;
    }

    /**
     * Gets the singleton instance of the name.
     *
     * @return instance
     */
    public static NeverMatchName get() {
        return INSTANCE;
    }
}

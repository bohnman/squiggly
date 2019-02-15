package com.github.bohnman.squiggly.core.name;

/**
 * Indicates that the node name matches any node at any nesting level.
 */
public class AnyDeepName implements SquigglyName {

    public static final String ID = "**";

    private static final AnyDeepName INSTANCE = new AnyDeepName();

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
        return 0;
    }

    @Override
    public boolean matches(String name) {
        return true;
    }

    /**
     * Gets the singleton instance of the name.
     *
     * @return instance
     */
    public static AnyDeepName get() {
        return INSTANCE;
    }
}

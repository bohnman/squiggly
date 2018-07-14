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
    public int match(String name) {
        return 0;
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

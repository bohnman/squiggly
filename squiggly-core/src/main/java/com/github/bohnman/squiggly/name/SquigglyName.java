package com.github.bohnman.squiggly.name;

/**
 * Represents a node name matching strategy.
 */
public interface SquigglyName {

    /**
     * Gets the name of the node.
     *
     * @return name
     */
    String getName();

    /**
     * Gets the raw value of the node.
     *
     * @return raw name
     */
    String getRawName();

    /**
     * Indicates how specific the name is.  The higher the value, the more specific.
     *
     * @return specificity
     */
    int getSpecificity();

    /**
     * Determines if the supplied name matches the current name.
     *
     * @param name a name
     * @return true if matches, false otherwise
     */
    boolean matches(String name);
}

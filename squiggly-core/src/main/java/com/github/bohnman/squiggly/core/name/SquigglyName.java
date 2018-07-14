package com.github.bohnman.squiggly.core.name;

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
     * Match this node against the supplied name.  A -1 indicates no match.  A positive integer represents a match.
     * The higher the positive integer, the more specificity there is to the match.
     *
     * @param name the name to match
     * @return -1 if no match, otherwise a positive integer
     */
    int match(String name);
}

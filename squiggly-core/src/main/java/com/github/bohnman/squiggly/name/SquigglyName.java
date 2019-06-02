package com.github.bohnman.squiggly.name;

/**
 * Represents a node name matching strategy.
 */
public interface SquigglyName {

    /**
     * Gets the token of the name.
     *
     * @return name
     */
    String getToken();

    /**
     * Indicates how specific the name is.  The higher the value, the more specific.
     *
     * @return specificity
     */
    int getSpecificity();

    /**
     * Determines if the supplied token matches the current token.
     *
     * @param token a token
     * @return true if matches, false otherwise
     */
    boolean matches(String token);
}

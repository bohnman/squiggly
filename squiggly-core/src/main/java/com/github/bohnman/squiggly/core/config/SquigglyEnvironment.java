package com.github.bohnman.squiggly.core.config;

/**
 * Specifies Squiggly's running envrionment.
 */
public enum SquigglyEnvironment {
    /**
     * Default environment.  Allows all Squiggly capabilities.
     */
    DEFAULT,

    /**
     * Secure environment.  Unsafe Squiggly operations will be restricted.
     */
    SECURE
}

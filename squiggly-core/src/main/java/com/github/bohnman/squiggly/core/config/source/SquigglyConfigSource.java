package com.github.bohnman.squiggly.core.config.source;

import javax.annotation.Nullable;

/**
 * A config source is an origin for configuration properties (file, environment, etc).
 */
public interface SquigglyConfigSource {

    /**
     * Retrieve the property associated with the supplied name or null if not found.
     *
     * @param name the property name
     * @return value or null if not found
     */
    @Nullable
    default String getProperty(String name) {
        return getProperty(name, null);
    }

    /**
     * Retrieve the property associated with the supplied name or the supplied default value if not found.
     *
     * @param name the property name
     * @param defaultValue default value
     * @return value or defaultValue
     */
    @Nullable
    String getProperty(String name, @Nullable String defaultValue);

    /**
     * Retrieve the origin of the value associated with the suplied name or null if not found.
     *
     * @param name name
     * @return origin
     */
    @Nullable
    String getOrigin(String name);
}

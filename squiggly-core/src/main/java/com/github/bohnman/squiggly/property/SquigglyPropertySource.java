package com.github.bohnman.squiggly.property;

import com.github.bohnman.core.lang.CoreObjects;

import javax.annotation.Nullable;

import static java.util.Objects.requireNonNull;

/**
 * A config source is an origin for configuration properties (file, environment, etc).
 */
public interface SquigglyPropertySource {

    /**
     * Retrieve the property associated with the supplied name or null if not found.
     *
     * @param name the property name
     * @return value or null if not found
     */
    @Nullable
    String findByName(String name);

    /**
     * Retrieve the property associated with the supplied name or the supplied default value if not found.
     *
     * @param name         the property name
     * @param defaultValue default value
     * @return value or defaultValue
     */
    default String findByName(String name, String defaultValue) {
        return CoreObjects.firstNonNull(findByName(name), requireNonNull(defaultValue));
    }

    /**
     * Retrieve the origin of the value associated with the suplied name or null if not found.
     *
     * @param name name
     * @return origin
     */
    @Nullable
    String findOriginByName(String name);
}

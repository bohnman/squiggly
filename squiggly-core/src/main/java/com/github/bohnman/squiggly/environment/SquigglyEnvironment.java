package com.github.bohnman.squiggly.environment;

import javax.annotation.Nullable;

import static java.util.Objects.requireNonNull;

public interface SquigglyEnvironment {

    @Nullable
    String getOrigin(String name);

    @Nullable
    String getProperty(String name);

    default String getProperty(String name, String defaultValue) {
        String value = getProperty(name);

        if (value == null) {
            value = requireNonNull(defaultValue);
        }

        return value;
    }

    default String getRequiredProperty(String name) throws IllegalStateException {
        String value = getProperty(name);

        if (value == null) {
            throw new IllegalStateException(String.format("Unable to find property with name [%s].", name));
        }

        return value;
    }

    @Nullable
    <T> T getProperty(String name, Class<T> type);

    default <T> T getProperty(String name, Class<T> type, T defaultValue) {
        T value = getProperty(name, type);

        if (value == null) {
            value = requireNonNull(defaultValue);
        }

        return value;
    }

    default <T> T getRequiredProperty(String name, Class<T> type) throws IllegalStateException {
        T value = getProperty(name, type);

        if (value == null) {
            throw new IllegalStateException(String.format("Unable to find property with name [%s].", name));
        }

        return value;
    }
}

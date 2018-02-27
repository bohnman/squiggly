package com.github.bohnman.squiggly.core.config.source;

import javax.annotation.Nullable;

public interface SquigglyConfigSource {

    @Nullable
    default String getProperty(String name) {
        return getProperty(name, null);
    }

    @Nullable
    String getProperty(String name, @Nullable String defaultValue);

    @Nullable
    String getLocation(String name);
}

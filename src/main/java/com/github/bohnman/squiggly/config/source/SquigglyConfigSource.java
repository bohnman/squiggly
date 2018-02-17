package com.github.bohnman.squiggly.config.source;

public interface SquigglyConfigSource {

    default String getProperty(String name) {
        return getProperty(name, null);
    }

   String getProperty(String name, String defaultValue);

    String getLocation(String name);
}

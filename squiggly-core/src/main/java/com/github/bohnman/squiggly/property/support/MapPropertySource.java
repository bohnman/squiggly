package com.github.bohnman.squiggly.property.support;

import com.github.bohnman.squiggly.property.SquigglyPropertySource;

import javax.annotation.Nullable;
import java.util.Map;

import static java.util.Objects.requireNonNull;

/**
 * Property source backed by a {@link java.util.Map}.
 */
public class MapPropertySource implements SquigglyPropertySource {

    private final String origin;
    private final Map<String, String> map;

    private MapPropertySource(String origin, Map<String, String> map) {
        this.origin = requireNonNull(origin);
        this.map = requireNonNull(map);
    }

    @Nullable
    @Override
    public String findByName(String name) {
        return map.get(name);
    }

    @Nullable
    @Override
    public String findOriginByName(String name) {
        return origin;
    }

    public static MapPropertySource create(Map<String, String> map) {
        return new MapPropertySource("map", map);
    }

    public static MapPropertySource create(String origin, Map<String, String> map) {
        return new MapPropertySource(origin, map);
    }
}

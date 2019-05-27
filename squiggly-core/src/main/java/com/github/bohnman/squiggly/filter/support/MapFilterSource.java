package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.squiggly.filter.SquigglyFilterSource;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import static java.util.Objects.requireNonNull;

/**
 * A filter repository backed by a map.
 */
public class MapFilterSource implements SquigglyFilterSource {

    private final Map<String, String> map;

    /**
     * Construct with the supplied map.
     *
     * @param map map whose key is the name and value is the filter
     */
    private MapFilterSource(Map<String, String> map) {
        this.map = requireNonNull(map);
    }

    @Nullable
    @Override
    public String findFilterByName(String name) {
        return map.get(name);
    }

    public static MapFilterSource create(Map<String, String> map) {
        return new MapFilterSource(Collections.unmodifiableMap(map));
    }
}

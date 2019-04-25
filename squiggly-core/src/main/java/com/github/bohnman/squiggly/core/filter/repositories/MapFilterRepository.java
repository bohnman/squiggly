package com.github.bohnman.squiggly.core.filter.repositories;

import com.github.bohnman.squiggly.core.filter.SquigglyFilterRepository;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

/**
 * A filter repository backed by a map.
 */
public class MapFilterRepository implements SquigglyFilterRepository {

    private final Map<String, String> filterByName;

    /**
     * Construct with an empty map.
     */
    public MapFilterRepository() {
        this.filterByName = Collections.emptyMap();
    }

    /**
     * Construct with the supplied map.
     *
     * @param filterByName map whose key is the name and value is the filter
     */
    public MapFilterRepository(Map<String, String> filterByName) {
        this.filterByName = Collections.unmodifiableMap(filterByName);
    }

    @Nullable
    @Override
    public String findByName(String name) {
        return filterByName.get(name);
    }
}

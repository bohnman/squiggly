package com.github.bohnman.squiggly.core.filter.repository;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

public class MapFilterRepository implements SquigglyFilterRepository {

    private final Map<String, String> filterByName;

    public MapFilterRepository() {
        this.filterByName = Collections.emptyMap();
    }

    public MapFilterRepository(Map<String, String> filterByName) {
        this.filterByName = Collections.unmodifiableMap(filterByName);
    }

    @Nullable
    @Override
    public String findByName(String name) {
        return filterByName.get(name);
    }
}

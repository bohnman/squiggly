package com.github.bohnman.squiggly.filter.repository;

import com.google.common.collect.ImmutableMap;

import java.util.Collections;
import java.util.Map;

public class MapFilterRepository implements SquigglyFilterRepository {

    private final Map<String, String> filterByName;

    public MapFilterRepository() {
        this.filterByName = Collections.emptyMap();
    }

    public MapFilterRepository(Map<String, String> filterByName) {
        this.filterByName = ImmutableMap.copyOf(filterByName);
    }

    @Override
    public String findByName(String name) {
        return filterByName.get(name);
    }
}

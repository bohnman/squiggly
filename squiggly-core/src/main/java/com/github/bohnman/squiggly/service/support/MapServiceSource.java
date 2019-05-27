package com.github.bohnman.squiggly.service.support;

import com.github.bohnman.squiggly.service.SquigglyServiceSource;

import javax.annotation.Nullable;
import java.util.Map;

public class MapServiceSource implements SquigglyServiceSource {

    private final Map<Object, Object> map;

    private MapServiceSource(Map<Object, Object> map) {
        this.map = map;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T findServiceByType(Class<T> type) {
        Object service = map.get(type);

        if (service == null) {
            return null;
        }

        if (!type.isAssignableFrom(service.getClass())) {
            return null;
        }

        return (T) service;
    }

    @Nullable
    @Override
    public Object findServiceByName(String name) {
        return map.get(name);
    }

    public static MapServiceSource create(Map<Object, Object> map) {
        return new MapServiceSource(map);
    }
}

package com.github.bohnman.squiggly.service;

import javax.annotation.Nullable;
import java.util.*;

import static java.util.Objects.requireNonNull;

public class SquigglyServices {

    private static final SquigglyServiceSource EMPTY_SOURCE = new SquigglyServiceSource() {
        @Nullable
        @Override
        public <T> T findServiceByType(Class<T> type) {
            return null;
        }

        @Nullable
        @Override
        public Object findServiceByName(String name) {
            return null;
        }
    };

    private SquigglyServices() {
    }

    public static SquigglyServiceSource compositeSource(SquigglyServiceSource... sources) {
        if (sources.length == 0) {
            return EMPTY_SOURCE;
        }

        if (sources.length == 1) {
            return sources[0];
        }

        return new CompositeServiceSource(Arrays.asList(sources));
    }

    public static SquigglyServiceSource compositeSource(List<SquigglyServiceSource> sources) {
        if (sources.isEmpty()) {
            return EMPTY_SOURCE;
        }

        if (sources.size() == 1) {
            return sources.get(0);
        }

        return new CompositeServiceSource(sources);
    }

    public static SquigglyServiceSource mapSource(Map<Object, Object> map) {
        if (map.isEmpty()) {
            return MapServiceSource.EMPTY;
        }

        if (map.size() == 1) {
            Map.Entry<Object, Object> entry = map.entrySet().iterator().next();
            return new MapServiceSource(Collections.singletonMap(entry.getKey(), entry.getValue()));
        }

        return new MapServiceSource(map);
    }

    private static class CompositeServiceSource implements SquigglyServiceSource {

        private final List<SquigglyServiceSource> sources;

        private CompositeServiceSource(List<SquigglyServiceSource> sources) {
            this.sources = requireNonNull(sources);
        }

        @Override
        public <T> T findServiceByType(Class<T> type) {
            for (SquigglyServiceSource source : sources) {
                T service = source.findServiceByType(type);

                if (service != null) {
                    return service;
                }
            }

            return null;
        }

        @Override
        public Object findServiceByName(String name) {
            for (SquigglyServiceSource source : sources) {
                Object service = source.findServiceByName(name);

                if (service != null) {
                    return service;
                }
            }

            return null;
        }
    }

    private static class MapServiceSource implements SquigglyServiceSource {

        private static final MapServiceSource EMPTY = new MapServiceSource(Collections.emptyMap());

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
    }
}

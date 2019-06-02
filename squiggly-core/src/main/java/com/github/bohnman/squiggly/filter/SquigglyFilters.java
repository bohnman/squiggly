package com.github.bohnman.squiggly.filter;

import javax.annotation.Nullable;
import java.util.*;

import static java.util.Objects.requireNonNull;

public class SquigglyFilters {

    private static final SquigglyFilterSource EMPTY_SOURCE = name -> null;

    private SquigglyFilters() {
    }

    public static SquigglyFilterSource compositeSource(SquigglyFilterSource... sources) {
        if (sources.length == 0) {
            return EMPTY_SOURCE;
        }

        if (sources.length == 1) {
            return sources[0];
        }

        return new CompositeFilterSource(Collections.unmodifiableList(Arrays.asList(sources)));
    }

    public static SquigglyFilterSource compositeSource(List<SquigglyFilterSource> sources) {
        if (sources.isEmpty()) {
            return EMPTY_SOURCE;
        }

        if (sources.size() == 1) {
            return sources.get(0);
        }

        return new CompositeFilterSource(Collections.unmodifiableList(new ArrayList<>(sources)));
    }

    public static SquigglyFilterSource mapSource(Map<String, String> map) {
        return new MapFilterSource(map);
    }

    public static SquigglyFilterSource staticSource(String filter) {
        return new StaticFilterSource(SquigglyFilterSource.CONTEXT_NAME, filter);
    }

    public static SquigglyFilterSource staticSource(String filterName, String filter) {
        return new StaticFilterSource(filterName, filter);
    }

    public static SquigglyFilterSource threadLocalSource() {
        return new ThreadLocalFilterSource(SquigglyFilterSource.CONTEXT_NAME);
    }

    public static SquigglyFilterSource threadLocalSource(String filterName) {
        return new ThreadLocalFilterSource(filterName);
    }

    /**
     * A filter source that finds the first source containing the filter name.
     */
    private static class CompositeFilterSource implements SquigglyFilterSource {

        private final List<SquigglyFilterSource> sources;

        private CompositeFilterSource(List<SquigglyFilterSource> sources) {
            this.sources = requireNonNull(sources);
        }

        @Nullable
        @Override
        public String findFilterByName(String name) {
            String filter = null;

            for (SquigglyFilterSource source : sources) {
                filter = source.findFilterByName(name);

                if (filter != null) {
                    break;
                }
            }

            return filter;
        }
    }

    /**
     * A filter repository backed by a map.
     */
    private static class MapFilterSource implements SquigglyFilterSource {

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

    private abstract static class SingleNamedFilterSource implements SquigglyFilterSource {

        private final String filterName;

        protected SingleNamedFilterSource(String filterName) {
            this.filterName = requireNonNull(filterName);
        }

        @Nullable
        @Override
        public final String findFilterByName(String name) {
            return filterName.equals(name) ? getFilter() : null;
        }

        @Nullable
        protected abstract String getFilter();
    }

    private static class StaticFilterSource extends SingleNamedFilterSource {

        private final String filter;

        private StaticFilterSource(String filterName, String filter) {
            super(filterName);
            this.filter = requireNonNull(filter);
        }

        @Override
        protected String getFilter() {
            return filter;
        }
    }

    private static class ThreadLocalFilterSource extends SingleNamedFilterSource {

        private ThreadLocalFilterSource(String filterName) {
            super(filterName);
        }

        @Override
        protected String getFilter() {
            return SquigglyFilterHolder.get();
        }
    }

}

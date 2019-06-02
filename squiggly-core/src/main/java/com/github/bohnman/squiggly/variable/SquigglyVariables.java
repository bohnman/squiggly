package com.github.bohnman.squiggly.variable;

import javax.annotation.Nullable;
import java.util.*;

public class SquigglyVariables {

    private static final SquigglyVariableSource EMPTY_SOURCE = name -> null;

    private SquigglyVariables() {
    }

    public static SquigglyVariableSource compositeSource(SquigglyVariableSource... sources) {
        if (sources.length == 0) {
            return EMPTY_SOURCE;
        }

        if (sources.length == 1) {
            return sources[0];
        }

        return new CompositeVariableSource(Arrays.asList(sources));
    }

    public static SquigglyVariableSource compositeSource(List<SquigglyVariableSource> sources) {
        if (sources.isEmpty()) {
            return EMPTY_SOURCE;
        }

        if (sources.size() == 1) {
            return sources.get(0);
        }

        return new CompositeVariableSource(sources);
    }

    public static SquigglyVariableSource mapSource(Map<String, Object> map) {
        if (map.isEmpty()) {
            return MapVariableSource.EMPTY;
        }

        if (map.size() == 1) {
            Map.Entry<String, Object> entry = map.entrySet().iterator().next();
            return new MapVariableSource(Collections.singletonMap(entry.getKey(), entry.getValue()));
        }

        return new MapVariableSource(map);
    }

    private static class CompositeVariableSource implements SquigglyVariableSource {

        private final List<SquigglyVariableSource> sources;

        private CompositeVariableSource(List<SquigglyVariableSource> sources) {
            this.sources = Objects.requireNonNull(sources);
        }

        @Nullable
        @Override
        public Object findVariableByName(String name) {
            Object value = null;

            for (SquigglyVariableSource source : sources) {
                value = source.findVariableByName(name);

                if (value != null) {
                    break;
                }
            }

            return value;
        }
    }

    private static class MapVariableSource implements SquigglyVariableSource {

        private static final MapVariableSource EMPTY = new MapVariableSource(Collections.emptyMap());

        private final Map<String, Object> variables;

        public MapVariableSource(Map<String, Object> variables) {
            this.variables = variables;
        }

        @Nullable
        @Override
        public Object findVariableByName(String name) {
            return variables.get(name);
        }
    }

    private static class ThreadLocalVariableSource implements SquigglyVariableSource {
        @Nullable
        @Override
        public Object findVariableByName(String name) {
            Map<String, Object> variables = SquigglyVariablesHolder.get();

            if (variables == null) {
                return null;
            }

            return variables.get(name);
        }
    }
}

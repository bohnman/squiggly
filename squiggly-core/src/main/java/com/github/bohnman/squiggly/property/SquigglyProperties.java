package com.github.bohnman.squiggly.property;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.*;

import static java.util.Objects.requireNonNull;

public class SquigglyProperties {

    private static final SquigglyPropertySource EMPTY_SOURCE = new SquigglyPropertySource() {
        @Nullable
        @Override
        public String findByName(String name) {
            return null;
        }

        @Nullable
        @Override
        public String findOriginByName(String name) {
            return null;
        }
    };

    private SquigglyProperties() {
    }

    public static SquigglyPropertySource compositeSource(SquigglyPropertySource... sources) {
        if (sources.length == 0) {
            return EMPTY_SOURCE;
        }

        if (sources.length == 1) {
            return sources[0];
        }

        return new CompositePropertySource(Arrays.asList(sources));
    }

    public static SquigglyPropertySource compositeSource(List<SquigglyPropertySource> sources) {
        if (sources.isEmpty()) {
            return EMPTY_SOURCE;
        }

        if (sources.size() == 1) {
            return sources.get(0);
        }

        return new CompositePropertySource(sources);
    }

    public static SquigglyPropertySource mapSource(Map<String, String> map) {
        return mapSource("map", map);
    }

    public static SquigglyPropertySource mapSource(String origin, Map<String, String> map) {
        if (map.isEmpty()) {
            return new MapPropertySource(origin, Collections.emptyMap());
        }

        if (map.size() == 1) {
            Map.Entry<String, String> entry = map.entrySet().iterator().next();
            return new MapPropertySource(origin, Collections.singletonMap(entry.getKey(), entry.getValue()));
        }

        return new MapPropertySource(origin, map);
    }

    public static SquigglyPropertySource propertiesSource(Properties properties) {
        return new PropertiesPropertySource("properties", properties);
    }

    public static SquigglyPropertySource propertiesSource(String origin, Properties properties) {
        return new PropertiesPropertySource(origin, properties);
    }

    public static SquigglyPropertySource propertiesSource(ClassLoader classLoader, String name) {
        return propertiesSource(classLoader.getResource(name));
    }

    public static SquigglyPropertySource propertiesSource(URL url) {
        Properties properties = new Properties();

        try (InputStream inputStream = url.openStream()) {
            properties.load(inputStream);
        } catch (IOException e) {
            throw new UncheckedIOException(String.format("Unable to load properties [%s]", url), e);
        }

        return new PropertiesPropertySource(url.toString(), properties);
    }

    private static class CompositePropertySource implements SquigglyPropertySource {

        private final List<SquigglyPropertySource> sources;

        private CompositePropertySource(List<SquigglyPropertySource> sources) {
            this.sources = requireNonNull(sources);
        }

        @Override
        @Nullable
        public String findByName(String name) {
            String property = null;

            for (SquigglyPropertySource source : sources) {
                property = source.findByName(name);

                if (property != null) {
                    break;
                }
            }

            return property;
        }

        @Nullable
        @Override
        public String findOriginByName(String name) {
            String origin = null;

            for (SquigglyPropertySource source : sources) {
                origin = source.findOriginByName(name);

                if (origin != null) {
                    break;
                }
            }

            return origin;
        }
    }

    /**
     * Property source backed by a {@link java.util.Map}.
     */
    private static class MapPropertySource implements SquigglyPropertySource {

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
    }

    /**
     * Config source that is back by a properties object.
     */
    private static class PropertiesPropertySource implements SquigglyPropertySource {

        private final Properties properties;
        private final String origin;


        private PropertiesPropertySource(String origin, Properties properties) {
            this.origin = requireNonNull(origin);
            this.properties = requireNonNull(properties);
        }

        @Nullable
        @Override
        public String findByName(String name) {
            return properties.getProperty(name);
        }

        @Nullable
        @Override
        public String findOriginByName(String name) {
            if (!properties.containsKey(name)) {
                return null;
            }
            return origin;
        }
    }
}

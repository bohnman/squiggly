package com.github.bohnman.squiggly.property.support;

import com.github.bohnman.squiggly.property.SquigglyPropertySource;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Properties;

import static java.util.Objects.requireNonNull;

/**
 * Config source that is back by a properties object.
 */
public class PropertiesPropertySource implements SquigglyPropertySource {

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

    public static PropertiesPropertySource create(Properties properties) {
        return new PropertiesPropertySource("properties", properties);
    }

    public static PropertiesPropertySource create(String origin, Properties properties) {
        return new PropertiesPropertySource(origin, properties);
    }

    public static PropertiesPropertySource create(URL url) {
        Properties properties = new Properties();

        try (InputStream inputStream = url.openStream()) {
            properties.load(inputStream);
        } catch (IOException e) {
            throw new UncheckedIOException(String.format("Unable to load properties [%s]", url), e);
        }

        return new PropertiesPropertySource(url.toString(), properties);
    }
}

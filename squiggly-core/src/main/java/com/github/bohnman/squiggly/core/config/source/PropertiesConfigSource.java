package com.github.bohnman.squiggly.core.config.source;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.Properties;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Config source that is back by a properties object.
 */
public class PropertiesConfigSource implements SquigglyConfigSource {

    private final Properties properties;
    private final String location;

    /**
     * Construct the source with an empty properties
     *
     * @param location a location name
     */
    public PropertiesConfigSource(String location) {
        this(location, new Properties());
    }

    /**
     * Construct the source with the supplied properties.
     *
     * @param location a location name
     * @param properties the proprerties
     */
    public PropertiesConfigSource(String location, Properties properties) {
        this.location = notNull(location);
        this.properties = notNull(properties);
    }

    /**
     * Construct the source using the supplied URL.
     *
     * @param url the url
     */
    public PropertiesConfigSource(URL url) {
        this(url.toString());

        try (InputStream inputStream = url.openStream()) {
            properties.load(inputStream);
        } catch (IOException e) {
            throw new UncheckedIOException(String.format("Unable to load properties [%s]", url), e);
        }
    }

    @Nullable
    @Override
    public String getProperty(String name, @Nullable String defaultValue) {
        String value = properties.getProperty(name);

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }

    @Nullable
    @Override
    public String getOrigin(String name) {
        if (!properties.containsKey(name)) {
            return null;
        }
        return location;
    }
}

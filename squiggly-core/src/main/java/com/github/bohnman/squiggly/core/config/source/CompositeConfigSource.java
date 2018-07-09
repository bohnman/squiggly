package com.github.bohnman.squiggly.core.config.source;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A config source that wraps other config sources in the order provided.
 */
public class CompositeConfigSource implements SquigglyConfigSource {

    private final Iterable<SquigglyConfigSource> sources;

    /**
     * Construct the composite with the provided sources.
     *
     * @param sources the config sources
     */
    public CompositeConfigSource(SquigglyConfigSource... sources) {
        this(Arrays.asList(sources));
    }

    /**
     * Construct the composide with the provided sources.
     *
     * @param sources the config sources
     */
    public CompositeConfigSource(Iterable<SquigglyConfigSource> sources) {
        this.sources = notNull(sources);
    }

    @Override
    @Nullable
    public String getProperty(String name, @Nullable String defaultValue) {
        String property = null;

        for (SquigglyConfigSource source : sources) {
            property = source.getProperty(name);

            if (property != null) {
                break;
            }
        }

        if (property == null) {
            property = defaultValue;
        }

        return property;
    }

    @Nullable
    @Override
    public String getOrigin(String name) {
        String location = null;

        for (SquigglyConfigSource source : sources) {
            location = source.getOrigin(name);

            if (location != null) {
                break;
            }
        }

        if (location == null) {
            location = "not found";
        }

        return location;
    }
}

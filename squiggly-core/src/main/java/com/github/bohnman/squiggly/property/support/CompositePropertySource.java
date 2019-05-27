package com.github.bohnman.squiggly.property.support;

import com.github.bohnman.squiggly.property.SquigglyPropertySource;

import javax.annotation.Nullable;
import java.util.List;

import static java.util.Objects.requireNonNull;

/**
 * A config source that wraps other config sources in the order provided.
 */
public class CompositePropertySource implements SquigglyPropertySource {

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

    public static CompositePropertySource create(List<SquigglyPropertySource> sources) {
        return new CompositePropertySource(sources);
    }
}

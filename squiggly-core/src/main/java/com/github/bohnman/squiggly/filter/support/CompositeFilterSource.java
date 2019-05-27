package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.squiggly.filter.SquigglyFilterSource;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;

import static java.util.Objects.requireNonNull;

/**
 * A filter source that finds the first source containing the filter name.
 */
public class CompositeFilterSource implements SquigglyFilterSource {

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

    public static CompositeFilterSource create(List<SquigglyFilterSource> sources) {
        return new CompositeFilterSource(sources);
    }
}

package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Variable source that is composed of multiple resolvers.  The first resolver to resolve the variable is the winner.
 */
public class CompositeVariableSource implements SquigglyVariableSource {

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

    public static CompositeVariableSource create(List<SquigglyVariableSource> sources) {
        return new CompositeVariableSource(Collections.unmodifiableList(sources));
    }
}

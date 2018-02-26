package com.github.bohnman.squiggly.core.variable;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.google.common.base.Preconditions.checkNotNull;

public class CompositeVariableResolver implements SquigglyVariableResolver {

    private final Iterable<SquigglyVariableResolver> resolvers;

    public CompositeVariableResolver(SquigglyVariableResolver... resolvers) {
        this(Arrays.asList(resolvers));
    }

    public CompositeVariableResolver(Iterable<SquigglyVariableResolver> resolvers) {
        this.resolvers = checkNotNull(resolvers);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name, @Nullable Object defaultValue) {
        Object value = null;


        for (SquigglyVariableResolver resolver : resolvers) {
            value = resolver.resolveVariable(name);

            if (value != null) {
                break;
            }
        }

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }
}

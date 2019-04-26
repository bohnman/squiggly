package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Variable resolver that is composed of multiple resolvers.  The first resolver to resolve the variable is the winner.
 */
public class CompositeVariableResolver implements SquigglyVariableResolver {

    private final Iterable<SquigglyVariableResolver> resolvers;

    /**
     * Constructor.
     *
     * @param resolvers resolvers
     */
    public CompositeVariableResolver(SquigglyVariableResolver... resolvers) {
        this(Arrays.asList(resolvers));
    }

    /**
     * Constructor.
     *
     * @param resolvers resolvers
     */
    public CompositeVariableResolver(Iterable<SquigglyVariableResolver> resolvers) {
        this.resolvers = notNull(resolvers);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name) {
        Object value = null;

        for (SquigglyVariableResolver resolver : resolvers) {
            value = resolver.resolveVariable(name);

            if (value != null) {
                break;
            }
        }

        return value;
    }
}

package com.github.bohnman.squiggly.context.provider;

import com.github.bohnman.squiggly.context.LazySquigglyContext;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.parser.SquigglyParser;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class AbstractSquigglyContextProvider implements SquigglyContextProvider {

    private final SquigglyParser parser;

    public AbstractSquigglyContextProvider() {
        this(new SquigglyParser());
    }

    public AbstractSquigglyContextProvider(SquigglyParser parser) {
        this.parser = parser;
    }

    @Override
    public SquigglyContext getContext() {
        return new LazySquigglyContext(parser, getFilter());
    }

    /**
     * Get the filter expression.
     *
     * @return filter expression
     */
    protected abstract String getFilter();


}

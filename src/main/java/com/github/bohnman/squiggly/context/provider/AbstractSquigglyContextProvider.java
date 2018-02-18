package com.github.bohnman.squiggly.context.provider;

import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.context.LazySquigglyContext;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.parser.SquigglyParser;

import javax.annotation.Nullable;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class AbstractSquigglyContextProvider implements SquigglyContextProvider {

    @Override
    public SquigglyContext getContext(Class beanClass, Squiggly squiggly) {
        return new LazySquigglyContext(beanClass, squiggly, getFilter(beanClass));
    }

    /**
     * Get the filter expression.
     *
     * @param beanClass class of the top-level bean being filtered
     * @return filter expression
     */
    @Nullable
    protected abstract String getFilter(Class beanClass);
}
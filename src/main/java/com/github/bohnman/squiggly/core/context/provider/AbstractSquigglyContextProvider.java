package com.github.bohnman.squiggly.core.context.provider;

import com.github.bohnman.squiggly.jackson.Squiggly;
import com.github.bohnman.squiggly.core.context.LazySquigglyContext;
import com.github.bohnman.squiggly.core.context.SquigglyContext;

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
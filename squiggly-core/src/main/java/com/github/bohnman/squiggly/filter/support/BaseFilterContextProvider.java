package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.squiggly.engine.SquigglyEngine;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterContext;

import javax.annotation.Nullable;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class BaseFilterContextProvider implements SquigglyFilterContextProvider {

    @Override
    public SquigglyFilterContext getContext(Class objectClass, SquigglyEngine engine) {
        return new LazySquigglyFilterContext(objectClass, engine, getFilter(objectClass));
    }

    /**
     * Get the filter expression.
     *
     * @param objectClass class of the top-level bean being filtered
     * @return filter expression
     */
    @Nullable
    protected String getFilter(Class objectClass) {
        return customizeFilter(provideFilter(objectClass), objectClass);
    }

    /**
     * Hook method to provide the filter based on the bean class
     *
     * @param objectClass any class
     * @return filter or null
     */
    @Nullable
    protected abstract String provideFilter(Class objectClass);

    /**
     * Hook method to customize the filte.  For example, wrap the filter in a nested filter.
     *
     * @param filter    the filter
     * @param objectClass the bean class
     * @return filte or null
     */
    @Nullable
    protected String customizeFilter(@Nullable String filter, @Nullable Class objectClass) {
        return filter;
    }
}
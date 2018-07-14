package com.github.bohnman.squiggly.core.context.provider;

import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.context.LazySquigglyContext;
import com.github.bohnman.squiggly.core.context.SquigglyContext;

import javax.annotation.Nullable;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class AbstractSquigglyContextProvider<S extends BaseSquiggly> implements SquigglyContextProvider<S> {

    @Override
    public SquigglyContext getContext(Class beanClass, S squiggly) {
        return new LazySquigglyContext(beanClass, squiggly, getFilter(beanClass));
    }

    /**
     * Get the filter expression.
     *
     * @param beanClass class of the top-level bean being filtered
     * @return filter expression
     */
    @Nullable
    protected String getFilter(Class beanClass) {
        return customizeFilter(provideFilter(beanClass), beanClass);
    }

    /**
     * Hook method to provide the filter based on the bean class
     *
     * @param beanClass any class
     * @return filter or null
     */
    @Nullable
    protected abstract String provideFilter(Class beanClass);

    /**
     * Hook method to customize the filte.  For example, wrap the filter in a nested filter.
     *
     * @param filter    the filter
     * @param beanClass the bean class
     * @return filte or null
     */
    @Nullable
    protected String customizeFilter(@Nullable String filter, @Nullable Class beanClass) {
        return filter;
    }
}
package com.github.bohnman.squiggly.core.filter.contextproviders;

import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.core.filter.contexts.LazySquigglyFilterContext;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterContext;

import javax.annotation.Nullable;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class AbstractFilterContextProvider<S extends BaseSquiggly> implements SquigglyFilterContextProvider<S> {

    @Override
    public SquigglyFilterContext getContext(Class beanClass, S squiggly) {
        return new LazySquigglyFilterContext(beanClass, squiggly, getFilter(beanClass));
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
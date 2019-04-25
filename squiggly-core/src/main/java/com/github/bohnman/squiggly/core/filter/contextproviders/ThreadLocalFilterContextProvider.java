package com.github.bohnman.squiggly.core.filter.contextproviders;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.filter.support.SquigglyFilterHolder;

import javax.annotation.Nullable;

/**
 * Retrieves a squiggly context via a thread local variable.
 *
 * @see SquigglyFilterHolder
 */
public class ThreadLocalFilterContextProvider extends AbstractFilterContextProvider {

    private final String defaultFilter;

    /**
     * Construct the provider with an empty default filter.
     */
    public ThreadLocalFilterContextProvider() {
        defaultFilter = null;
    }

    /**
     * Construct the provider with the supplied default filter.
     *
     * @param defaultFilter the default filter if one is not set
     */
    public ThreadLocalFilterContextProvider(String defaultFilter) {
        this.defaultFilter = defaultFilter;
    }

    @Nullable
    @Override
    protected String provideFilter(Class beanClass) {
        return CoreObjects.firstNonNull(SquigglyFilterHolder.get(), defaultFilter);
    }

    @Override
    public boolean isFilteringEnabled() {
        return CoreObjects.firstNonNull(SquigglyFilterHolder.get(), defaultFilter) != null;
    }
}

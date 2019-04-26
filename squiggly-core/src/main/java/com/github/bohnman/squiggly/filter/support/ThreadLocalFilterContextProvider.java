package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.core.lang.CoreObjects;

import javax.annotation.Nullable;

/**
 * Retrieves a squiggly context via a thread local variable.
 *
 * @see SquigglyFilterHolder
 */
public class ThreadLocalFilterContextProvider extends BaseFilterContextProvider {

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
    protected String provideFilter(Class objectClass) {
        return CoreObjects.firstNonNull(SquigglyFilterHolder.get(), defaultFilter);
    }

    @Override
    public boolean isFilteringEnabled() {
        return CoreObjects.firstNonNull(SquigglyFilterHolder.get(), defaultFilter) != null;
    }
}

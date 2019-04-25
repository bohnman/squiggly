package com.github.bohnman.squiggly.core.filter.contextproviders;

import com.github.bohnman.squiggly.core.name.names.AnyDeepName;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;

/**
 * Provider implementation that just takes a fixed filter expression.
 */
@ThreadSafe
public class SimpleFilterContextProvider extends AbstractFilterContextProvider {

    private final String filter;

    /**
     * Construct with no filter.
     */
    public SimpleFilterContextProvider() {
        this(null);
    }

    /**
     * Construct with the supplied filter.
     *
     * @param filter the filter to use
     */
    public SimpleFilterContextProvider(@Nullable String filter) {
        this.filter = filter;
    }

    @Override
    public boolean isFilteringEnabled() {
        if (filter == null) {
            return false;
        }

        if (AnyDeepName.ID.equals(filter)) {
            return false;
        }

        return true;
    }

    @Override
    protected String provideFilter(Class beanClass) {
        return filter;
    }
}

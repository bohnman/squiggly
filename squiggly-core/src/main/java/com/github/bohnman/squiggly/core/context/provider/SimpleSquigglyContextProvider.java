package com.github.bohnman.squiggly.core.context.provider;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;

/**
 * Provider implementation that just takes a fixed filter expression.
 */
@ThreadSafe
public class SimpleSquigglyContextProvider extends AbstractSquigglyContextProvider {

    private final String filter;

    /**
     * Construct with no filter.
     */
    public SimpleSquigglyContextProvider() {
        this(null);
    }

    /**
     * Construct with the supplied filter.
     *
     * @param filter the filter to use
     */
    public SimpleSquigglyContextProvider(@Nullable String filter) {
        this.filter = filter;
    }

    @Override
    public boolean isFilteringEnabled() {
        if (filter == null) {
            return false;
        }

        if ("**".equals(filter)) {
            return false;
        }

        return true;
    }

    @Override
    protected String provideFilter(Class beanClass) {
        return filter;
    }
}

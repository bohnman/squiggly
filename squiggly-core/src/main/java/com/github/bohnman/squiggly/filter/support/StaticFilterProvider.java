package com.github.bohnman.squiggly.filter.support;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;

/**
 * Provider implementation that just takes a fixed filter expression.
 */
@ThreadSafe
public class StaticFilterProvider extends BaseFilterProvider {

    private final String filter;

    /**
     * Construct with no filter.
     */
    public StaticFilterProvider() {
        this(null);
    }

    /**
     * Construct with the supplied filter.
     *
     * @param filter the filter to use
     */
    public StaticFilterProvider(@Nullable String filter) {
        this.filter = filter;
    }


    @Override
    protected String provideFilter(Class<?> objectClass) {
        return filter;
    }
}

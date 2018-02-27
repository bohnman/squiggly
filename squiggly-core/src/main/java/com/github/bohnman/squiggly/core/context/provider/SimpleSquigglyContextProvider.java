package com.github.bohnman.squiggly.core.context.provider;

import net.jcip.annotations.ThreadSafe;

import javax.annotation.Nullable;

/**
 * Provider implementation that just takes a fixed filter expression.
 */
@ThreadSafe
public class SimpleSquigglyContextProvider extends AbstractSquigglyContextProvider {

    private final String filter;

    public SimpleSquigglyContextProvider() {
        this(null);
    }

    public SimpleSquigglyContextProvider(@Nullable  String filter) {
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

    @Nullable
    @Override
    protected String getFilter(Class beanClass) {
        return filter;
    }
}

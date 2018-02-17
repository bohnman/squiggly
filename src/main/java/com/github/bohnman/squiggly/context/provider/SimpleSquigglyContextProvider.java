package com.github.bohnman.squiggly.context.provider;

import net.jcip.annotations.ThreadSafe;

/**
 * Provider implementation that just takes a fixed filter expression.
 */
@ThreadSafe
public class SimpleSquigglyContextProvider extends AbstractSquigglyContextProvider {

    private final String filter;

    public SimpleSquigglyContextProvider(String filter) {
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
    protected String getFilter(Class beanClass) {
        return filter;
    }
}

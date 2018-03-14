package com.github.bohnman.squiggly.core.context.provider;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.holder.SquigglyFilterHolder;

import javax.annotation.Nullable;

public class ThreadLocalSquigglyContextProvider extends AbstractSquigglyContextProvider {

    private final String defaultFilter;

    public ThreadLocalSquigglyContextProvider() {
        defaultFilter = null;
    }

    public ThreadLocalSquigglyContextProvider(String defaultFilter) {
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

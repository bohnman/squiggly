package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.squiggly.filter.SquigglyFilterSource;

import javax.annotation.Nullable;

public abstract class ContextFilterSource implements SquigglyFilterSource {

    public static final String NAME = "__context__";

    @Override
    public String findFilterByName(String name) {
        if (NAME.equals(name)) {
            return getFilter();
        }

        return null;
    }

    @Nullable
    protected abstract String getFilter();
}

package com.github.bohnman.squiggly.filter.support;

public class StaticContextFilterSource extends ContextFilterSource {

    private final String filter;

    private StaticContextFilterSource(String filter) {
        this.filter = filter;
    }

    @Override
    protected String getFilter() {
        return filter;
    }

    public static StaticContextFilterSource create(String filter) {
        return new StaticContextFilterSource(filter);
    }
}

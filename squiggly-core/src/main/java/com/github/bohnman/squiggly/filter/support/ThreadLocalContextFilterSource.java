package com.github.bohnman.squiggly.filter.support;

public class ThreadLocalContextFilterSource extends ContextFilterSource {

    private ThreadLocalContextFilterSource() {
    }

    @Override
    protected String getFilter() {
        return SquigglyFilterHolder.get();
    }

    public static ThreadLocalContextFilterSource create() {
        return new ThreadLocalContextFilterSource();
    }
}

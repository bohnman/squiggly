package com.github.bohnman.squiggly.filter.support;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.engine.SquigglyEngine;
import com.github.bohnman.squiggly.filter.SquigglyFilterContext;
import com.github.bohnman.squiggly.parse.support.FilterNode;

import javax.annotation.Nullable;
import javax.annotation.concurrent.NotThreadSafe;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Squiggly context that loads the parsed nodes on demand.
 */
@NotThreadSafe
public class LazySquigglyFilterContext implements SquigglyFilterContext {

    private final Class beanClass;
    private final String filter;
    private final SquigglyEngine squiggly;

    @Nullable
    private FilterNode filterNode;

    /**
     * Construct the context with base information.
     *
     * @param beanClass bean class
     * @param squiggly  squiggly configurator
     * @param filter    filter
     */
    public LazySquigglyFilterContext(Class beanClass, SquigglyEngine squiggly, String filter) {
        this.beanClass = notNull(beanClass);
        this.squiggly = notNull(squiggly);
        notNull(filter);
        this.filter = notNull(CoreObjects.firstNonNull(squiggly.getFilterRepository().findByName(filter), filter));
    }

    public Class getBeanClass() {
        return beanClass;
    }

    @Override
    public FilterNode getParsedFilter() {
        if (filterNode == null) {
            filterNode = squiggly.getParser().parsePropertyFilter(filter);
        }

        return filterNode;
    }

    @Override
    public String getFilter() {
        return filter;
    }
}

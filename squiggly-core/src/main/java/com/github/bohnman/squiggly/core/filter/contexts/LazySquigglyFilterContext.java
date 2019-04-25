package com.github.bohnman.squiggly.core.filter.contexts;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterContext;
import com.github.bohnman.squiggly.core.parse.nodes.FilterNode;

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
    private final BaseSquiggly squiggly;

    @Nullable
    private FilterNode filterNode;

    /**
     * Construct the context with base information.
     *
     * @param beanClass bean class
     * @param squiggly  squiggly configurator
     * @param filter    filter
     */
    public LazySquigglyFilterContext(Class beanClass, BaseSquiggly squiggly, String filter) {
        this.beanClass = notNull(beanClass);
        this.squiggly = notNull(squiggly);
        notNull(filter);
        this.filter = notNull(CoreObjects.firstNonNull(squiggly.getFilterRepository().findByName(filter), filter));
    }

    @Override
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

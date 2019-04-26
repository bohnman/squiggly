package com.github.bohnman.squiggly.filter;

import com.github.bohnman.squiggly.BaseSquiggly;

/**
 * Used for supplying a parseable context
 */
public interface SquigglyFilterContextProvider<S extends BaseSquiggly> {

    /**
     * Get the context.
     *
     * @param beanClass the class of the top-level bean being filtered
     * @param squiggly  squiggly object
     * @return context
     */
    SquigglyFilterContext getContext(Class beanClass, S squiggly);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return true if enabled, false if not
     */
    default boolean isFilteringEnabled() {
        return true;
    }
}

package com.github.bohnman.squiggly.core.context.provider;

import com.github.bohnman.squiggly.jackson.Squiggly;
import com.github.bohnman.squiggly.core.context.SquigglyContext;
import com.github.bohnman.squiggly.jackson.filter.SquigglyPropertyFilter;

/**
 * Used for supplying a @{@link SquigglyPropertyFilter} with a way to retrieve a
 * context.
 */
public interface SquigglyContextProvider {

    /**
     * Get the context.
     *
     * @param beanClass the class of the top-level bean being filtered
     * @param squiggly squiggly object
     * @return context
     */
    SquigglyContext getContext(Class beanClass, Squiggly squiggly);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return true if enabled, false if not
     */
    default boolean isFilteringEnabled() {
        return true;
    }
}

package com.github.bohnman.squiggly.core.context.provider;

import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.context.SquigglyContext;

/**
 * Used for supplying a parseable context
 */
public interface SquigglyContextProvider<S extends BaseSquiggly> {

    /**
     * Get the context.
     *
     * @param beanClass the class of the top-level bean being filtered
     * @param squiggly  squiggly object
     * @return context
     */
    SquigglyContext getContext(Class beanClass, S squiggly);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return true if enabled, false if not
     */
    default boolean isFilteringEnabled() {
        return true;
    }
}

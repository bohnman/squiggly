package com.github.bohnman.squiggly.filter;

import com.github.bohnman.squiggly.engine.SquigglyEngine;

/**
 * Used for supplying a parseable context
 */
public interface SquigglyFilterContextProvider {

    /**
     * Get the context.
     *
     * @param objectClass the class of the top-level bean being filtered
     * @param engine    squiggly object
     * @return context
     */
    SquigglyFilterContext getContext(Class objectClass, SquigglyEngine engine);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return true if enabled, false if not
     */
    default boolean isFilteringEnabled() {
        return true;
    }
}


// Squiggly.init(engine, modules)

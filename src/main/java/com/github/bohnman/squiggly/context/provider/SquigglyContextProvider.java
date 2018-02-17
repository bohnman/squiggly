package com.github.bohnman.squiggly.context.provider;

import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.parser.SquigglyParser;

/**
 * Used for supplying a @{@link com.github.bohnman.squiggly.filter.SquigglyPropertyFilter} with a way to retrieve a
 * context.
 */
public interface SquigglyContextProvider {

    /**
     * Get the context.
     *
     * @param parser parser
     * @param beanClass the class of the top-level bean being filtered
     * @return context
     */
    SquigglyContext getContext(SquigglyParser parser, Class beanClass);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return true if enabled, false if not
     */
    default boolean isFilteringEnabled() {
        return true;
    }
}

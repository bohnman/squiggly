package com.github.bohnman.squiggly.filter;

import com.github.bohnman.squiggly.parse.SquigglyParser;

import javax.annotation.Nullable;

/**
 * Used for supplying a parseable context
 */
public interface SquigglyFilterProvider {

    /**
     * Get the context.
     *
     * @param objectClass      the class of the top-level bean being filtered
     * @param objectRepository object repository
     * @return context
     */
    @Nullable
    String getFilter(Class<?> objectClass);
}


// Squiggly.init(engine, modules)

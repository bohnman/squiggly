package com.github.bohnman.squiggly.filter;

import javax.annotation.Nullable;

/**
 * A filter repository stores filters by a name.
 */
@FunctionalInterface
public interface SquigglyFilterSource {

    String CONTEXT_NAME = "__CONTEXT__";

    /**
     * Retrieve a filter matching the supplied name or null if not found.
     *
     * @param name a name
     * @return filter or null
     */
    @Nullable
    String findFilterByName(String name);

}

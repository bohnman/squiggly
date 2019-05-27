package com.github.bohnman.squiggly.filter;

import javax.annotation.Nullable;

/**
 * A filter repository stores filters by a name.
 */
public interface SquigglyFilterSource {

    /**
     * Retrieve a filter matching the supplied name or null if not found.
     *
     * @param name a name
     * @return filter or null
     */
    @Nullable
    String findFilterByName(String name);
}

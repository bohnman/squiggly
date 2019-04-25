package com.github.bohnman.squiggly.core.filter;

import javax.annotation.Nullable;

/**
 * A filter repository stores filters by a name.
 */
public interface SquigglyFilterRepository {

    /**
     * Retrieve a filter matching the supplied name or null if not found.
     *
     * @param name a name
     * @return filter or null
     */
    @Nullable
    String findByName(String name);
}

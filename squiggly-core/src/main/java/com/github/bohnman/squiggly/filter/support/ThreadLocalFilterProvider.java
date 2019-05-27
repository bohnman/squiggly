package com.github.bohnman.squiggly.filter.support;

import javax.annotation.Nullable;

/**
 * Retrieves a squiggly context via a thread local variable.
 *
 * @see SquigglyFilterHolder
 */
public class ThreadLocalFilterProvider extends BaseFilterProvider {

    @Nullable
    @Override
    protected String provideFilter(Class<?> objectClass) {
        return SquigglyFilterHolder.get();
    }
}

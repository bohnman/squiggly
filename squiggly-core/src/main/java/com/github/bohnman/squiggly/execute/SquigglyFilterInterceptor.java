package com.github.bohnman.squiggly.execute;

import javax.annotation.Nullable;

@FunctionalInterface
public interface SquigglyFilterInterceptor {

    @Nullable
    String applyFilter(SquigglyPathContext context, String filter);
}

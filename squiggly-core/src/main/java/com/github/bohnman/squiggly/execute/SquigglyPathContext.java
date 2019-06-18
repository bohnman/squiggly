package com.github.bohnman.squiggly.execute;

import com.github.bohnman.squiggly.path.SquigglyObjectPath;

public interface SquigglyPathContext {

    SquigglyObjectPath getPath();

    default String getName() {
        return getPath().getName();
    }

    default Class<?> getType() {
        return getPath().getType();
    }
}

package com.github.bohnman.squiggly.path;

import javax.annotation.Nullable;

public interface SquigglyObjectPath extends Iterable<SquigglyObjectPath> {

    SquigglyObjectPath append(String name, Class<?> type);

    String getName();

    @Nullable
    SquigglyObjectPath getParent();

    @Nullable
    SquigglyObjectPath getRoot();

    Class<?> getType();
}

package com.github.bohnman.squiggly.core.filter.repository;

import javax.annotation.Nullable;

public interface SquigglyFilterRepository {

    @Nullable
    String findByName(String name);
}

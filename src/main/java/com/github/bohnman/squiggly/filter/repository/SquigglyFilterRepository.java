package com.github.bohnman.squiggly.filter.repository;

import javax.annotation.Nullable;

public interface SquigglyFilterRepository {

    @Nullable
    String findByName(String name);
}

package com.github.bohnman.squiggly.function.repository;

import com.github.bohnman.squiggly.function.SquigglyFunction;

import javax.annotation.Nullable;

public interface SquigglyFunctionRepository {

    @Nullable
    SquigglyFunction<Object> findByName(String name);
}

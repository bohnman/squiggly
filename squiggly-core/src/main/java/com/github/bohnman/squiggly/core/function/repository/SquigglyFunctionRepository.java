package com.github.bohnman.squiggly.core.function.repository;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;

import java.util.List;

public interface SquigglyFunctionRepository {

    List<SquigglyFunction<Object>> findByName(String name);
}

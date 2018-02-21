package com.github.bohnman.squiggly.function.repository;

import com.github.bohnman.squiggly.function.SquigglyFunction;

import javax.annotation.Nullable;
import java.util.List;

public interface SquigglyFunctionRepository {

    List<SquigglyFunction<Object>> findByName(String name);
}

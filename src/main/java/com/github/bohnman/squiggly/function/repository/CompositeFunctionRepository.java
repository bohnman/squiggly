package com.github.bohnman.squiggly.function.repository;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.google.common.collect.ImmutableList;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;

public class CompositeFunctionRepository implements SquigglyFunctionRepository {

    private List<SquigglyFunctionRepository> repositories;

    public CompositeFunctionRepository(SquigglyFunctionRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    public CompositeFunctionRepository(List<SquigglyFunctionRepository> repositories) {
        this.repositories = ImmutableList.copyOf(repositories);
    }

    @Nullable
    @Override
    public SquigglyFunction<Object> findByName(String name) {
        SquigglyFunction<Object> function = null;

        for (SquigglyFunctionRepository repository : repositories) {
            function = repository.findByName(name);

            if (function != null) {
                break;
            }
        }

        return function;
    }
}

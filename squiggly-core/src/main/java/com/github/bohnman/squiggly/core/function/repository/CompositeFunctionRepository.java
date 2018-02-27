package com.github.bohnman.squiggly.core.function.repository;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class CompositeFunctionRepository implements SquigglyFunctionRepository {

    private List<SquigglyFunctionRepository> repositories;

    public CompositeFunctionRepository(SquigglyFunctionRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    public CompositeFunctionRepository(List<SquigglyFunctionRepository> repositories) {
        this.repositories = Collections.unmodifiableList(repositories);
    }

    @Override
    public List<SquigglyFunction<Object>> findByName(String name) {
        List<SquigglyFunction<Object>> functions = Collections.emptyList();

        for (SquigglyFunctionRepository repository : repositories) {
            functions = repository.findByName(name);

            if (!functions.isEmpty()) {
                break;
            }
        }

        return functions;
    }
}

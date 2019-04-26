package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionRepository;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A function repo containing other function repos.
 */
public class CompositeFunctionRepository implements SquigglyFunctionRepository {

    private List<SquigglyFunctionRepository> repositories;

    /**
     * Construct with supplied repos.
     *
     * @param repositories the repos
     */
    public CompositeFunctionRepository(SquigglyFunctionRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    /**
     * Construct with supplied repos.
     *
     * @param repositories the repos
     */
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

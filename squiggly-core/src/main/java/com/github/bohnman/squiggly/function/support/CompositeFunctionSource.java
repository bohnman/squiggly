package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionSource;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A function repo containing other function repos.
 */
public class CompositeFunctionSource implements SquigglyFunctionSource {

    private List<SquigglyFunctionSource> repositories;

    /**
     * Construct with supplied repos.
     *
     * @param repositories the repos
     */
    public CompositeFunctionSource(SquigglyFunctionSource... repositories) {
        this(Arrays.asList(repositories));
    }

    /**
     * Construct with supplied repos.
     *
     * @param repositories the repos
     */
    public CompositeFunctionSource(List<SquigglyFunctionSource> repositories) {
        this.repositories = Collections.unmodifiableList(repositories);
    }

    @Override
    public List<SquigglyFunction<Object>> findByName(String name) {
        List<SquigglyFunction<Object>> functions = Collections.emptyList();

        for (SquigglyFunctionSource repository : repositories) {
            functions = repository.findByName(name);

            if (!functions.isEmpty()) {
                break;
            }
        }

        return functions;
    }
}

package com.github.bohnman.squiggly.function;

import java.util.List;

/**
 * A function store.
 */
public interface SquigglyFunctionRepository {

    /**
     * Find all functions matching the supplied name.
     *
     * @param name function name
     * @return matching functions
     */
    List<SquigglyFunction<Object>> findByName(String name);
}

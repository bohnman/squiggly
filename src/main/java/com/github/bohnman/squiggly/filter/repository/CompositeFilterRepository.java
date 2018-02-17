package com.github.bohnman.squiggly.filter.repository;

import java.util.Arrays;

import static com.google.common.base.Preconditions.checkNotNull;

public class CompositeFilterRepository implements SquigglyFilterRepository {

    private final Iterable<SquigglyFilterRepository> repositories;

    public CompositeFilterRepository(SquigglyFilterRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    public CompositeFilterRepository(Iterable<SquigglyFilterRepository> repositories) {
        this.repositories = checkNotNull(repositories);
    }

    @Override
    public String findByName(String name) {
        String filter = null;

        for (SquigglyFilterRepository repository : repositories) {
            filter = repository.findByName(name);

            if (filter != null) {
                break;
            }
        }

        return filter;
    }
}

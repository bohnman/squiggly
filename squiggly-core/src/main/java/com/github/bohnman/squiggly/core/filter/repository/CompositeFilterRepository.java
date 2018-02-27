package com.github.bohnman.squiggly.core.filter.repository;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class CompositeFilterRepository implements SquigglyFilterRepository {

    private final Iterable<SquigglyFilterRepository> repositories;

    public CompositeFilterRepository(SquigglyFilterRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    public CompositeFilterRepository(Iterable<SquigglyFilterRepository> repositories) {
        this.repositories = notNull(repositories);
    }

    @Nullable
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

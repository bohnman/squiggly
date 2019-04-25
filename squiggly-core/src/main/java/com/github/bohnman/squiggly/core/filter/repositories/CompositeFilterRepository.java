package com.github.bohnman.squiggly.core.filter.repositories;

import com.github.bohnman.squiggly.core.filter.SquigglyFilterRepository;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A filter repository that finds the first repository containing the filter name.
 */
public class CompositeFilterRepository implements SquigglyFilterRepository {

    private final Iterable<SquigglyFilterRepository> repositories;

    /**
     * Constructor.
     *
     * @param repositories the filter repositories to use
     */
    public CompositeFilterRepository(SquigglyFilterRepository... repositories) {
        this(Arrays.asList(repositories));
    }

    /**
     * Constructor.
     *
     * @param repositories the filter repositories to use
     */
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

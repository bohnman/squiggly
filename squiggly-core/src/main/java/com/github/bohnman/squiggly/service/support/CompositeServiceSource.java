package com.github.bohnman.squiggly.service.support;

import com.github.bohnman.squiggly.service.SquigglyServiceSource;

import java.util.List;

import static java.util.Objects.requireNonNull;

public class CompositeServiceSource implements SquigglyServiceSource {

    private final List<SquigglyServiceSource> sources;

    private CompositeServiceSource(List<SquigglyServiceSource> sources) {
        this.sources = requireNonNull(sources);
    }

    @Override
    public <T> T findServiceByType(Class<T> type) {
        for (SquigglyServiceSource source : sources) {
            T service = source.findServiceByType(type);

            if (service != null) {
                return service;
            }
        }

        return null;
    }

    @Override
    public Object findServiceByName(String name) {
        for (SquigglyServiceSource source : sources) {
            Object service = source.findServiceByName(name);

            if (service != null) {
                return service;
            }
        }

        return null;
    }

    public static CompositeServiceSource create(List<SquigglyServiceSource> sources) {
        return new CompositeServiceSource(sources);
    }
}

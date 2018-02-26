package com.github.bohnman.core.collect;

import java.util.List;
import java.util.stream.Collectors;

public class CoreLists {

    public CoreLists() {
    }

    @SuppressWarnings("unchecked")
    public static <T> List<T> of (Iterable<? extends T> iterable) {
        if (iterable instanceof List) {
            return (List<T>) iterable;
        }

        return CoreStreams.of(iterable).collect(Collectors.toList());
    }
}

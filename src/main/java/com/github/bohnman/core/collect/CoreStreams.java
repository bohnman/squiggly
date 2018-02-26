package com.github.bohnman.core.collect;

import java.util.Collection;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class CoreStreams {

    private CoreStreams() {

    }

    public static <T> Stream<T> of(Iterable<T> iterable) {
        return (iterable instanceof Collection)
                ? ((Collection<T>) iterable).stream()
                : StreamSupport.stream(iterable.spliterator(), false);
    }
}

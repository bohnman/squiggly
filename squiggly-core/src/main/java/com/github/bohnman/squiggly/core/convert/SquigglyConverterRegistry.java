package com.github.bohnman.squiggly.core.convert;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

public interface SquigglyConverterRegistry {

    List<ConverterRecord> findAll();

    void add(ConverterRecord record);

    default void addAll(Collection<ConverterRecord> records) {
        records.forEach(this::add);
    }

    default <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter) {
        add(new ConverterRecord(source, target, converter));
    }

    default <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter, int order) {
        add(new ConverterRecord(source, target, converter, order));
    }

}

package com.github.bohnman.squiggly.core.convert;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

/**
 * A converter registry is a read/write store for converter records.
 *
 * @see ConverterRecord
 */
public interface SquigglyConverterRegistry {

    /**
     * Retrieve all the converters in the registry.
     *
     * @return converter records
     */
    List<ConverterRecord> findAll();

    /**
     * Register a single convert record.
     *
     * @param record the converter record
     */
    void add(ConverterRecord record);

    /**
     * Register mutiple converter records.
     *
     * @param records converter records
     */
    default void addAll(Collection<ConverterRecord> records) {
        records.forEach(this::add);
    }

    /**
     * Register a single converter record with normal ordering.
     *
     * @param source    source type
     * @param target    target type
     * @param converter converter function
     * @param <S>       source
     * @param <T>       target
     */
    default <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter) {
        add(new ConverterRecord(source, target, converter));
    }

    /**
     * Register a single converter record with the supplied ordering.
     *
     * @param source    source type
     * @param target    target type
     * @param converter converter function
     * @param order     the order
     * @param <S>       source
     * @param <T>       target
     */
    default <S, T> void add(Class<S> source, Class<T> target, Function<S, T> converter, int order) {
        add(new ConverterRecord(source, target, converter, order));
    }

}

package com.github.bohnman.squiggly.convert;

import java.util.function.Function;

/**
 * A registry for converters.
 */
public interface SquigglyConverterRegistry {

    /**
     * Register a single converter record with normal ordering.
     *
     * @param source    source type
     * @param target    target type
     * @param converter converter function
     * @param <S>       source
     * @param <T>       target
     */
    default <S, T> void addConverter(Class<S> source, Class<T> target, Function<S, T> converter) {
        addConverter(source, target, ConverterDescriptor.Order.NORMAL, converter);
    }
    /**
     * Register a single converter record with the supplied ordering.
     *
     * @param source    source type
     * @param target    target type
     * @param order     the order
     * @param converter converter function
     * @param <S>       source
     * @param <T>       target
     */
    <S, T> void addConverter(Class<S> source, Class<T> target, int order, Function<S, T> converter);

}

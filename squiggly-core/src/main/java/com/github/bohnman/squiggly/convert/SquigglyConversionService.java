package com.github.bohnman.squiggly.convert;

import javax.annotation.Nullable;

/**
 * Provides conversion capabilites between source and target types.
 */
public interface SquigglyConversionService {

    /**
     * Finds a converter record that matches the source and targe ttypes.
     *
     * @param source source type
     * @param target target type
     * @return converter record or null
     */
    @Nullable
    ConverterDescriptor findRecord(Class<?> source, Class<?> target);

    /**
     * Determines if the service an convert between the source and target types.
     *
     * @param source source type
     * @param target target type
     * @return true/false
     */
    boolean canConvert(Class<?> source, Class<?> target);

    /**
     * Attempt to convert an object to the target type.
     *
     * @param value  any object
     * @param target target type
     * @param <T>    target
     * @return converted value
     * @throws IllegalArgumentException if it cannot be converter
     */
    <T> T convert(Object value, Class<T> target) throws IllegalArgumentException;
}

package com.github.bohnman.squiggly.convert;

public interface SquigglyConversionService {

    boolean canConvert(Class<?> source, Class<?> target);

    <T> T convert(Object value, Class<T> target);
}

package com.github.bohnman.squiggly.core.convert;

import javax.annotation.Nullable;

public interface SquigglyConversionService {

    @Nullable
    ConverterRecord findRecord(Class<?> source, Class<?> target);

    boolean canConvert(Class<?> source, Class<?> target);

    <T> T convert(Object value, Class<T> target);
}

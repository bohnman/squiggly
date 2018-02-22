package com.github.bohnman.squiggly.convert;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.function.Function;

public class JodaConverters {

    public static List<ConverterRecord> get() {
        return ImmutableList.of(

        );
    }

    private static <S, T> ConverterRecord nullSafe(Class<S> source, Class<T> target, Function<S, T> converter) {
        return new NullSafeConverterRecord(source, target, converter);
    }
}

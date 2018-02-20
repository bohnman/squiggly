package com.github.bohnman.squiggly.convert;

import java.util.function.Function;

public abstract class NullSafeConverter<S, T> implements Function<S, T> {

    @Override
    public T apply(S source) {
        if (source == null) {
            return null;
        }

        return doApply(source);
    }

    protected abstract T doApply(S s);

    public static <S, T> Function<S, T> wrap(Function<S, T> converter) {
        return new NullSafeConverter<S, T>() {
            @Override
            protected T doApply(S s) {
                return converter.apply(s);
            }
        };
    }
}

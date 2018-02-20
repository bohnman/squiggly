package com.github.bohnman.squiggly.convert;

import java.util.function.Function;

import static com.google.common.base.Preconditions.checkNotNull;

public class NullSafeConverterRecord extends ConverterRecord {

    public NullSafeConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter) {
        super(source, target, converter);
    }

    @Override
    public Object apply(Object o) {
        if (o == null) {
            return null;
        }

        return super.apply(o);
    }
}

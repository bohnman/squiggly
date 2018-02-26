package com.github.bohnman.squiggly.core.convert;

import java.util.function.Function;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class ConverterRecord implements Function<Object, Object> {

    private final Class<?> source;
    private final Class<?> target;
    private final Function<?, ?> converter;

    public ConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter) {
        this.source = notNull(source);
        this.target = notNull(target);
        this.converter = notNull(converter);
    }

    public Class<?> getSource() {
        return source;
    }

    public Class<?> getTarget() {
        return target;
    }

    public Function<?, ?> getConverter() {
        return converter;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object apply(Object o) {
        return ((Function) converter).apply(o);
    }
}

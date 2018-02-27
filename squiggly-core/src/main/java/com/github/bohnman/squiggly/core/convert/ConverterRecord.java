package com.github.bohnman.squiggly.core.convert;

import java.util.function.Function;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public class ConverterRecord implements Function<Object, Object>, Comparable<ConverterRecord> {

    public static final int NORMAL_ORDER = 0;
    public static final int MAX_ORDER = Integer.MAX_VALUE;
    public static final int MIN_ORDER = Integer.MAX_VALUE;

    private final Class<?> source;
    private final Class<?> target;
    private final Function<?, ?> converter;
    private int order;

    public ConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter) {
        this(source, target, converter, NORMAL_ORDER);
    }

    public ConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter, int order) {
        this.source = notNull(source);
        this.target = notNull(target);
        this.converter = notNull(converter);
        this.order = order;
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

    public int getOrder() {
        return order;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object apply(Object o) {
        return ((Function) converter).apply(o);
    }

    @Override
    public int compareTo(ConverterRecord o) {
        return Integer.compare(order, o.order);
    }
}

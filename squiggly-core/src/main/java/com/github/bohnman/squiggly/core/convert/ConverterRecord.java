package com.github.bohnman.squiggly.core.convert;

import java.util.function.Function;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A metadata class that hold information on how to convert between two types.
 */
public class ConverterRecord implements Function<Object, Object>, Comparable<ConverterRecord> {

    public static final int NORMAL_ORDER = 0;
    public static final int MAX_ORDER = Integer.MAX_VALUE;
    public static final int MIN_ORDER = Integer.MAX_VALUE;

    private final Class<?> source;
    private final Class<?> target;
    private final Function<?, ?> converter;
    private int order;

    /**
     * Construct with using an order of {@value NORMAL_ORDER}.
     *
     * @param source source type
     * @param target target type
     * @param converter converter function
     */
    public ConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter) {
        this(source, target, converter, NORMAL_ORDER);
    }

    /**
     * Construct with the supplied order.
     *
     * @param source source type
     * @param target target type
     * @param converter converter function
     * @param order the order
     */
    public ConverterRecord(Class<?> source, Class<?> target, Function<?, ?> converter, int order) {
        this.source = notNull(source);
        this.target = notNull(target);
        this.converter = notNull(converter);
        this.order = order;
    }

    /**
     * Get the source (from) type.
     *
     * @return source type
     */
    public Class<?> getSource() {
        return source;
    }

    /**
     * Get the target (to) type.
     *
     * @return target type
     */
    public Class<?> getTarget() {
        return target;
    }

    /**
     * Get the converter function.
     *
     * @return converter function
     */
    public Function<?, ?> getConverter() {
        return converter;
    }

    /**
     * Get the order.  This is used to help decide among multiple converters of the same type.
     *
     * @return order
     */
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

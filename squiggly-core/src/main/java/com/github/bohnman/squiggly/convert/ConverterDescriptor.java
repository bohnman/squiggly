package com.github.bohnman.squiggly.convert;

import java.util.Objects;

import static java.util.Objects.requireNonNull;

/**
 * A metadata class that hold information on how to convert between two types.
 */
public class ConverterDescriptor implements Comparable<ConverterDescriptor> {

    private final Class<?> source;
    private final Class<?> target;
    private final int order;

    /**
     * Construct with using an order of {@value NORMAL_ORDER}.
     *
     * @param source    source type
     * @param target    target type
     */
    public ConverterDescriptor(Class<?> source, Class<?> target) {
        this(source, target, Order.NORMAL);
    }

    /**
     * Construct with the supplied order.
     *
     * @param source    source type
     * @param target    target type
     * @param order     the order
     */
    public ConverterDescriptor(Class<?> source, Class<?> target, int order) {
        this.source = requireNonNull(source);
        this.target = requireNonNull(target);
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
     * Get the order.  This is used to help decide among multiple converters of the same type.
     *
     * @return order
     */
    public int getOrder() {
        return order;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ConverterDescriptor that = (ConverterDescriptor) o;
        return order == that.order &&
                source.equals(that.source) &&
                target.equals(that.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, target, order);
    }

    @Override
    public int compareTo(ConverterDescriptor o) {
        return Integer.compare(order, o.order);
    }

    public static class Order {
        public static final int NORMAL = 0;
        public static final int MIN = Integer.MAX_VALUE;
        public static final int MAX = Integer.MAX_VALUE;

        private Order() {
        }
    }
}

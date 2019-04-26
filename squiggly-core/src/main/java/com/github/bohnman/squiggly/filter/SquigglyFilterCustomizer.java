package com.github.bohnman.squiggly.filter;

import java.util.function.BiFunction;

/**
 * Convenience interface to customize a filter.
 */
@FunctionalInterface
public interface SquigglyFilterCustomizer extends BiFunction<String, Class<?>, String> {

    /**
     * Perform the customization using the supplied filter and bean class.
     *
     * @param filter    filter
     * @param beanClass bean class
     * @return customized filter
     */
    @Override
    String apply(String filter, Class<?> beanClass);
}

package com.github.bohnman.squiggly.core.filter;

import java.util.function.BiFunction;

/**
 * Filter customizer utilities.
 */
public class SquilggyFilterCustomizers {

    private SquilggyFilterCustomizers() {
    }

    /**
     * Create a filter customizer using the supplied bi-function.
     *
     * @param function the bi-function
     * @return customizer
     */
    public static SquigglyFilterCustomizer create(BiFunction<String, Class<?>, String> function) {
        //noinspection NullableProblems
        return function::apply;
    }

    /**
     * Wrap the filter with the supplied prefix and suffix, if it is a descendant of the supplied class.
     *
     * @param targetBeanClass class to match
     * @param prefix          filter prefix
     * @param suffix          filter suffix
     * @return customizer
     */
    public static SquigglyFilterCustomizer wrap(Class<?> targetBeanClass, String prefix, String suffix) {
        return (filter, beanClass) -> {
            if (filter != null && targetBeanClass.isAssignableFrom(beanClass)) {
                filter = prefix + filter + suffix;
            }
            return filter;
        };
    }

}

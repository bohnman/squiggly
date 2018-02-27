package com.github.bohnman.squiggly.core.filter;

import java.util.function.BiFunction;

public class SquilggyFilterCustomizers {

    private SquilggyFilterCustomizers() {
    }

    public static SquigglyFilterCustomizer create(BiFunction<String, Class<?>, String> function) {
        return function::apply;
    }

    public static SquigglyFilterCustomizer wrap(Class<?> targetBeanClass, String prefix, String suffix) {
        return (filter, beanClass) -> {
            if (filter != null && targetBeanClass.isAssignableFrom(beanClass)) {
                filter = prefix + filter + suffix;
            }
            return filter;
        };
    }

}

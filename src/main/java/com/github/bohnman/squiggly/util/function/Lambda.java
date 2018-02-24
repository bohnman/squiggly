package com.github.bohnman.squiggly.util.function;

import java.util.function.Function;
import java.util.function.Predicate;

public interface Lambda extends Function<Object[], Object>, Predicate<Object[]> {

    default Object invoke(Object... arguments) {
        return apply(arguments);
    }

    @Override
    Object apply(Object[] arguments);

    @Override
    default boolean test(Object[] arguments) {
        Object result = apply(arguments);

        if (result == null) {
            return false;
        }

        if (result instanceof Boolean) {
            return (Boolean) result;
        }

        if (result instanceof Number) {
            return ((Number) result).doubleValue() != 0;
        }

        if (result instanceof String) {
            return !"".equals(result);
        }

        return true;
    }
}

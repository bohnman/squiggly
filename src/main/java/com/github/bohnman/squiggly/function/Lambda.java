package com.github.bohnman.squiggly.function;

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
        return CoreFunctions.toBoolean(result);
    }
}

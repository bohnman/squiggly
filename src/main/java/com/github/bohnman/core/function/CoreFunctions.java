package com.github.bohnman.core.function;

import java.util.function.Function;
import java.util.function.Predicate;

public class CoreFunctions {

    private CoreFunctions() {
    }

    public static <T, R> Function<T,R> nullSafe(Function<T, R> function) {
        return (input) -> input == null ? null: function.apply(input);
    }

    public static <T> Function<T, Boolean> from(Predicate<T> predicate) {
        return predicate::test;
    }
}

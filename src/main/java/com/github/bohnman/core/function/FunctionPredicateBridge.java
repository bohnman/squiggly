package com.github.bohnman.core.function;

import com.github.bohnman.core.convert.CoreConversions;

import java.util.function.Function;
import java.util.function.Predicate;

public interface FunctionPredicateBridge<S, R> extends Function<S, R>, Predicate<S> {

    @Override
    default boolean test(S s) {
        return CoreConversions.toBoolean(apply(s));
    }
}

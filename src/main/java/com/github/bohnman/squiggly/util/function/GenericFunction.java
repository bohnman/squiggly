package com.github.bohnman.squiggly.util.function;

import com.github.bohnman.squiggly.util.SquigglyUtils;

import java.util.function.Function;
import java.util.function.Predicate;

import static com.github.bohnman.squiggly.util.SquigglyUtils.toBoolean;

public interface GenericFunction<S, R> extends Function<S, R>, Predicate<S> {

    @Override
    default boolean test(S s) {
        return SquigglyUtils.toBoolean(apply(s));
    }
}

package com.github.bohnman.core.function;

import javax.annotation.Nullable;
import java.util.function.Predicate;

public interface CoreProperty extends FunctionPredicateBridge<Object, Object>, Predicate<Object> {

    boolean isAscending();

    @Nullable
    default Object get(@Nullable Object object) {
        return apply(object);
    }
}
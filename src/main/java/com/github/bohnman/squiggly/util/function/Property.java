package com.github.bohnman.squiggly.util.function;

import java.util.function.Predicate;

public interface Property extends GenericFunction<Object, Object>, Predicate<Object> {

    boolean isAscending();

    default Object get(Object object) {
        return apply(object);
    }
}
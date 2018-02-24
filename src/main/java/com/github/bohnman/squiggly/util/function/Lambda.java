package com.github.bohnman.squiggly.util.function;

import com.github.bohnman.squiggly.util.array.ArrayWrappers;

public interface Lambda extends GenericFunction<Object, Object> {

    Object invoke(Object... arguments);

    @Override
    default Object apply(Object arguments) {
        if (arguments == null) {
            return invoke(new Object[]{null});
        }

        if (arguments instanceof Object[]) {
            return invoke((Object[]) arguments);
        }

        if (arguments.getClass().isArray()) {
            invoke(ArrayWrappers.create(arguments).toArray());
        }

        return invoke(arguments);
    }

}

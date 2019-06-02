package com.github.bohnman.squiggly.variable;

import javax.annotation.Nullable;

@FunctionalInterface
public interface SquigglyVariableSource {

    @Nullable
    Object findVariableByName(String name);

    @Nullable
    default Object findVariableByName(String name, @Nullable Object defaultValue) {
        Object value = findVariableByName(name);

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }
}

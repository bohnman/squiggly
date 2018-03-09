package com.github.bohnman.squiggly.core.variable;

import javax.annotation.Nullable;

public interface SquigglyVariableResolver {

    @Nullable
    Object resolveVariable(String name);

    @Nullable
   default Object resolveVariable(String name, @Nullable Object defaultValue) {
        Object value = resolveVariable(name);

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }
}

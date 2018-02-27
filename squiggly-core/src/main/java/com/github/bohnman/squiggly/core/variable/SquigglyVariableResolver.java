package com.github.bohnman.squiggly.core.variable;

import javax.annotation.Nullable;

public interface SquigglyVariableResolver {

    @Nullable
    default Object resolveVariable(String name) {
        return resolveVariable(name, null);
    }

    @Nullable
    Object resolveVariable(String name, @Nullable Object defaultValue);
}

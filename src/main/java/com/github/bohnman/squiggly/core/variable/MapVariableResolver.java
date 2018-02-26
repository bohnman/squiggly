package com.github.bohnman.squiggly.core.variable;

import com.google.common.collect.ImmutableMap;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

public class MapVariableResolver implements SquigglyVariableResolver {

    private final Map<String, Object> variables;

    public MapVariableResolver() {
        this.variables = Collections.emptyMap();
    }

    public MapVariableResolver(Map<String, Object> variables) {
        this.variables = ImmutableMap.copyOf(variables);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name, @Nullable Object defaultValue) {
        Object value = variables.get(name);

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }
}

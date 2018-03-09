package com.github.bohnman.squiggly.core.variable;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

public class MapVariableResolver implements SquigglyVariableResolver {

    private final Map<String, Object> variables;

    public MapVariableResolver() {
        this.variables = Collections.emptyMap();
    }

    public MapVariableResolver(Map<String, Object> variables) {
        this.variables = Collections.unmodifiableMap(variables);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name) {
        return variables.get(name);
    }
}

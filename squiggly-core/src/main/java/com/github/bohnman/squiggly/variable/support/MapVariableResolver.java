package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

/**
 * Variable resolver backed by a map.
 */
public class MapVariableResolver implements SquigglyVariableResolver {

    private final Map<String, Object> variables;

    /**
     * Constructor using an empty map.
     */
    public MapVariableResolver() {
        this.variables = Collections.emptyMap();
    }

    /**
     * Constructor with the provided map.
     *
     * @param variables map
     */
    public MapVariableResolver(Map<String, Object> variables) {
        this.variables = Collections.unmodifiableMap(variables);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name) {
        return variables.get(name);
    }
}

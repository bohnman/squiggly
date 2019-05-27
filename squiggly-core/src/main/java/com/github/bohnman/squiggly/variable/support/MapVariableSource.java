package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Map;

import static java.util.Objects.requireNonNull;

/**
 * Variable resolver backed by a map.
 */
public class MapVariableSource implements SquigglyVariableSource {

    private final Map<String, Object> variables;

    /**
     * Constructor using an empty map.
     */
    private MapVariableSource() {
        this.variables = requireNonNull(Collections.emptyMap());
    }

    /**
     * Constructor with the provided map.
     *
     * @param variables map
     */
    public MapVariableSource(Map<String, Object> variables) {
        this.variables = Collections.unmodifiableMap(variables);
    }

    @Nullable
    @Override
    public Object findVariableByName(String name) {
        return variables.get(name);
    }

    public static MapVariableSource create(Map<String, Object> variables) {
        return new MapVariableSource(Collections.unmodifiableMap(variables));
    }
}

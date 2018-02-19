package com.github.bohnman.squiggly.function.repository;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.google.common.collect.ImmutableMap;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class MapFunctionRepository implements SquigglyFunctionRepository {

    private final ImmutableMap<String, SquigglyFunction<Object>> functionMap;

    public MapFunctionRepository(SquigglyFunction... functions) {
        this(Arrays.asList(functions));
    }

    @SuppressWarnings("unchecked")
    public <T> MapFunctionRepository(Iterable<SquigglyFunction> functions) {
        Map<String, SquigglyFunction<Object>> functionMap = new HashMap<>();

        for (SquigglyFunction<?> function : functions) {
            functionMap.put(function.getName(), (SquigglyFunction) function);

            for (String alias : function.getAliases()) {
                functionMap.put(alias, (SquigglyFunction) function);
            }
        }

        this.functionMap = ImmutableMap.copyOf(functionMap);
    }

    @Nullable
    @Override
    public SquigglyFunction<Object> findByName(String name) {
        return functionMap.get(name);
    }
}

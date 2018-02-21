package com.github.bohnman.squiggly.function.repository;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimaps;

import java.util.Arrays;
import java.util.List;

public class MapFunctionRepository implements SquigglyFunctionRepository {

    private final ListMultimap<String, SquigglyFunction<Object>> functionMap;

    public MapFunctionRepository(SquigglyFunction<?>... functions) {
        this(Arrays.asList(functions));
    }

    @SuppressWarnings("unchecked")
    public <T> MapFunctionRepository(Iterable<SquigglyFunction<?>> functions) {
        ListMultimap<String, SquigglyFunction<Object>> functionMap = ArrayListMultimap.create();

        for (SquigglyFunction<?> function : functions) {
            functionMap.put(function.getName(), (SquigglyFunction) function);

            for (String alias : function.getAliases()) {
                functionMap.put(alias, (SquigglyFunction) function);
            }
        }

        this.functionMap = Multimaps.unmodifiableListMultimap(functionMap);
    }

    @Override
    public List<SquigglyFunction<Object>> findByName(String name) {
        return functionMap.get(name);
    }
}

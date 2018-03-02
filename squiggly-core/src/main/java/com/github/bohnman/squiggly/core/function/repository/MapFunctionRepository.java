package com.github.bohnman.squiggly.core.function.repository;

import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.stream.Collectors.*;

@SuppressWarnings("unchecked")
public class MapFunctionRepository implements SquigglyFunctionRepository {

    private final Map<String, List<SquigglyFunction<Object>>> functionMap;

    public MapFunctionRepository(SquigglyFunction<?>... functions) {
        this(Arrays.asList(functions));
    }

    public <T> MapFunctionRepository(Iterable<SquigglyFunction<?>> functions) {
        Map<String, List<SquigglyFunction<Object>>> functionMap = (Map) CoreStreams.of(functions)
                .flatMap(f -> Stream.concat(Stream.of(toPair(f.getName().toLowerCase(), f)), f.getAliases().stream().map(a -> toPair(a, f))))
                .collect(groupingBy(CorePair::getLeft, mapping(CorePair::getRight, toList())));

        this.functionMap = Collections.unmodifiableMap(functionMap);
    }

    private CorePair<String, SquigglyFunction<?>> toPair(String name, SquigglyFunction<?> function) {
        return CorePair.of(name, function);
    }

    @Override
    public List<SquigglyFunction<Object>> findByName(String name) {
        return CoreObjects.firstNonNull(functionMap.get(name.toLowerCase()), Collections.emptyList());
    }
}

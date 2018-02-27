package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

public class CollectionFunctions {
    private CollectionFunctions() {
    }

    @SquigglyMethod(aliases = "where")
    public static Object filter(Object value, CoreLambda coreLambda) {
        if (value == null || coreLambda == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);

            return IntStream.range(0, wrapper.size())
                    .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i)))
                    .mapToObj(wrapper::get)
                    .toArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);

        return IntStream.range(0, list.size())
                .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(list.get(i), i)))
                .mapToObj((IntFunction<Object>) list::get)
                .collect(toList());
    }

    @SquigglyMethod(aliases = "where")
    @SuppressWarnings("unchecked")
    public static Object filter(Object value, Predicate predicate) {
        if (value == null || predicate == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            return wrapper.stream().filter((Predicate<Object>) predicate).toArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);

        return list.stream()
                .filter(predicate)
                .collect(toList());
    }

    @SquigglyMethod
    public static Object map(Object value, CoreLambda coreLambda) {
        if (value == null || coreLambda == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);

            return IntStream.range(0, wrapper.size())
                    .mapToObj(i -> coreLambda.invoke(wrapper.get(i), i))
                    .toArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);

        return IntStream.range(0, list.size())
                .mapToObj(i -> coreLambda.invoke(list.get(i), i))
                .collect(toList());
    }

    @SuppressWarnings("unchecked")
    @SquigglyMethod
    public static Object map(Object value, Function function) {
        if (value == null || function == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            return wrapper.stream().map((Function<Object, Object>) function).toArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);

        return list.stream()
                .map(function)
                .collect(toList());
    }


    @SquigglyMethod
    public static Object first(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            String string = (String) value;
            return string.isEmpty() ? "" : string.substring(0, 1);
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int len = wrapper.size();
            return len == 0 ? null : wrapper.get(0);
        }

        if (value instanceof Iterable) {
            Iterable iterable = (Iterable) value;
            return CoreIterables.getFirst(iterable, null);
        }

        return value;
    }

    @SquigglyMethod
    public static Object last(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            String string = (String) value;
            return string.isEmpty() ? "" : string.substring(string.length() - 1);
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int len = wrapper.size();
            return len == 0 ? null : wrapper.get(len - 1);
        }

        if (value instanceof Iterable) {
            Iterable iterable = (Iterable) value;
            return CoreIterables.getLast(iterable, null);
        }

        return value;
    }
}

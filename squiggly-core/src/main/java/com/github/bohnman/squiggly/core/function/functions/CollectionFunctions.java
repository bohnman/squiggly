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
import java.util.Map;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@SuppressWarnings("unchecked")
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
                .collect(Collectors.toList());
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
                .collect(Collectors.toList());
    }

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
                .collect(Collectors.toList());
    }

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
                .collect(Collectors.toList());
    }

    public static Object[] toArray(Object value) {
        if (value == null) {
            return new Object[]{};
        }

        if (value instanceof List) {
            return ((List) value).toArray();
        }

        if (value instanceof Object[]) {
            return (Object[]) value;
        }

        if (value.getClass().isArray()) {
            return CoreArrays.wrap(value).toArray();
        }

        if (value instanceof Iterable) {
            return CoreLists.of((Iterable) value).toArray();
        }

        return new Object[]{value};
    }

    public static List<Object> toList(Object value) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof List) {
            return (List) value;
        }

        if (value.getClass().isArray()) {
            return CoreArrays.wrap(value);
        }

        if (value instanceof Iterable) {
            return CoreLists.of((Iterable) value);
        }

        return Collections.singletonList(value);
    }

    public static Map<?, ?> toMap(Object value) {
        if (value == null) {
            return Collections.emptyMap();
        }

        if (value instanceof Map) {
            return (Map) value;
        }

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);
            return IntStream.range(0, list.size())
                    .boxed()
                    .collect(Collectors.toMap(Integer.class::cast, (Function<Integer, Object>) list::get));
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            return IntStream.range(0, wrapper.size())
                    .boxed()
                    .collect(Collectors.toMap(Integer.class::cast, wrapper::get));
        }

        return MixedFunctions.toMap(value);
    }
}

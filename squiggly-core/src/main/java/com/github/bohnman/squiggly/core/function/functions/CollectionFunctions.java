package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.squiggly.core.function.ValueHandler;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@SuppressWarnings("unchecked")
public class CollectionFunctions {
    private CollectionFunctions() {
    }

    @SquigglyFunctionMethod(aliases = "where")
    public static Object filter(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new ValueHandler<Object>(coreLambda) {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i)))
                        .mapToObj(wrapper::get)
                        .toArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                return IntStream.range(0, list.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(list.get(i), i)))
                        .mapToObj(list::get)
                        .collect(Collectors.toList());
            }

            @Override
            protected Object handleObject(Object value) {
                return handleList(Collections.singletonList(value));
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "where")
    @SuppressWarnings("unchecked")
    public static Object filter(Object value, Predicate predicate) {
        if (predicate == null) {
            return Collections.emptyList();
        }

        return new ValueHandler<Object>(predicate) {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper.stream().filter((Predicate<Object>) predicate).toArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                return list.stream()
                        .filter(predicate)
                        .collect(Collectors.toList());
            }

            @Override
            protected Object handleObject(Object value) {
                return handleList(Collections.singletonList(value));
            }
        }.handle(value);
    }

    public static Object first(Object value) {
        return new ValueHandler<Object>() {
            @Override
            protected Object handleObject(Object value) {
                return value;
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                int len = wrapper.size();
                return len == 0 ? null : wrapper.get(0);
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                return CoreIterables.getFirst(iterable, null);
            }

            @Override
            protected Object handleString(String string) {
                return string.isEmpty() ? "" : string.substring(0, 1);
            }
        }.handle(value);
    }

    public static Object last(Object value) {
        return new ValueHandler<Object>() {
            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                int len = wrapper.size();
                return len == 0 ? null : wrapper.get(len - 1);
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                return CoreIterables.getLast(iterable, null);
            }

            @Override
            protected Object handleObject(Object value) {
                return value;
            }

            @Override
            protected Object handleString(String string) {
                return string.isEmpty() ? "" : string.substring(string.length() - 1);
            }
        }.handle(value);
    }

    public static Object map(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new ValueHandler<Object>(coreLambda) {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> coreLambda.invoke(wrapper.get(i), i))
                        .toArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                return IntStream.range(0, list.size())
                        .mapToObj(i -> coreLambda.invoke(list.get(i), i))
                        .collect(Collectors.toList());
            }

            @Override
            protected Object handleObject(Object value) {
                return handleList(Collections.singletonList(value));
            }
        }.handle(value);

    }

    public static Object map(Object value, Function function) {
        if (function == null) {
            return Collections.emptyList();
        }

        return new ValueHandler<Object>(function) {
            @Override
            protected Object handleNull() {
                return super.handleNull();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper.stream().map((Function<Object, Object>) function).toArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                return list.stream()
                        .map(function)
                        .collect(Collectors.toList());
            }

            @Override
            protected Object handleObject(Object value) {
                return handleList(Collections.singletonList(value));
            }
        }.handle(value);
    }

    public static Object[] toArray(Object value) {
        return new ValueHandler<Object[]>() {
            @Override
            protected Object[] handleNull() {
                return new Object[0];
            }

            @Override
            protected Object[] handleArrayWrapper(CoreArrayWrapper wrapper) {
                return CoreArrays.wrap(value).toArray();
            }

            @Override
            protected Object[] handleList(List<Object> list) {
                return list.toArray();
            }

            @Override
            protected Object[] handleObject(Object value) {
                return new Object[]{value};
            }
        }.handle(value);
    }

    public static List<Object> toList(Object value) {
        return new ValueHandler<List<Object>>() {
            @Override
            protected List<Object> handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected List<Object> handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper;
            }

            @Override
            protected List<Object> handleList(List<Object> list) {
                return list;
            }

            @Override
            protected List<Object> handleObject(Object value) {
                return Collections.singletonList(value);
            }
        }.handle(value);
    }

    public static Map<?, ?> toMap(Object value) {
        return new ValueHandler<Map<?, ?>>() {
            @Override
            protected Map<?, ?> handleNull() {
                return Collections.emptyMap();
            }

            @Override
            protected Map<?, ?> handleArrayWrapper(CoreArrayWrapper wrapper) {
                return IntStream.range(0, wrapper.size())
                        .boxed()
                        .collect(Collectors.toMap(Integer.class::cast, wrapper::get));
            }

            @Override
            protected Map<?, ?> handleList(List<Object> list) {
                return IntStream.range(0, list.size())
                        .boxed()
                        .collect(Collectors.toMap(Integer.class::cast, list::get));
            }

            @Override
            protected Map<?, ?> handleMap(Map<Object, Object> map) {
                return map;
            }

            @Override
            protected Map<?, ?> handleObject(Object value) {
                return MixedFunctions.toMap(value);
            }
        }.handle(value);
    }
}

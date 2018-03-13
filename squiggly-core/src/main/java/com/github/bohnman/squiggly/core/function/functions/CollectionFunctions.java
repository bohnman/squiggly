package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.core.function.value.BaseCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.value.BaseStreamingCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.value.CollectionReturningValueHandler;
import com.github.bohnman.squiggly.core.function.value.ValueHandler;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings("unchecked")
public class CollectionFunctions {
    private CollectionFunctions() {
    }

    public static boolean all(Object value, Predicate predicate) {
        if (predicate == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream();
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.allMatch(predicate);
            }
        }.handle(value);
    }

    public static boolean all(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(lambda) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return (Stream) IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .filter(bool -> bool);
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.count() == wrapper.size();
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "some")
    public static boolean any(Object value, Predicate predicate) {
        if (predicate == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream();
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.anyMatch(predicate);
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "some")
    public static boolean any(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(lambda) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return (Stream) IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .filter(bool -> bool);
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findAny().orElse(null) != null;
            }
        }.handle(value);
    }

    public static boolean none(Object value, Predicate predicate) {
        if (predicate == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream();
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.noneMatch(predicate);
            }
        }.handle(value);
    }

    public static boolean none(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return false;
        }

        return new BaseStreamingCollectionValueHandler<Boolean>(lambda) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return (Stream) IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .filter(bool -> bool);
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findAny().orElse(null) == null;
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "where")
    public static Object filter(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "where")
    @SuppressWarnings("unchecked")
    public static Object filter(Object value, Predicate predicate) {
        if (predicate == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().filter((Predicate<Object>) predicate);
            }
        }.handle(value);
    }


    public static Object reject(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> !CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

        }.handle(value);
    }

    public static Object reject(Object value, Predicate predicate) {
        if (predicate == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().filter((Predicate<Object>) predicate.negate());
            }
        }.handle(value);
    }


    public static Object find(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return null;
        }

        return new BaseStreamingCollectionValueHandler<Object>(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    public static Object find(Object value, Predicate predicate) {
        if (predicate == null) {
            return null;
        }

        return new BaseStreamingCollectionValueHandler<Object>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().filter((Predicate<Object>) predicate);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    public static Object findLast(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return null;
        }

        return new BaseStreamingCollectionValueHandler<Object>(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                List list =  stream.collect(Collectors.toList());
                return list.isEmpty() ? null: list.get(list.size() - 1);
            }
        }.handle(value);
    }

    public static Object findLast(Object value, Predicate predicate) {
        if (predicate == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                for (int i = wrapper.size() -1; i >= 0; i--) {
                    if (predicate.test(wrapper.get(i))) {
                        return Stream.of(wrapper.get(i));
                    }
                }

                return Stream.empty();
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return (Integer) stream.findFirst().orElse(-1);
            }
        }.handle(value);
    }

    public static int findLastIndex(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(i -> i);
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                List list =  stream.collect(Collectors.toList());
                return list.isEmpty() ? -1 : (Integer) list.get(list.size() - 1);
            }
        }.handle(value);
    }

    public static int findLastIndex(Object value, Predicate predicate) {
        if (predicate == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                for (int i = wrapper.size() -1; i >= 0; i--) {
                    if (predicate.test(wrapper.get(i))) {
                        return Stream.of(i);
                    }
                }

                return Stream.empty();
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return (Integer) stream.findFirst().orElse(-1);
            }
        }.handle(value);
    }

    public static int findIndex(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(coreLambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(coreLambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(i -> i);
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return (Integer) stream.findFirst().orElse(-1);
            }
        }.handle(value);
    }

    public static int findIndex(Object value, Predicate predicate) {
        if (predicate == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(predicate) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                for (int i = 0; i < wrapper.size(); i++) {
                    if (predicate.test(wrapper.get(i))) {
                        return Stream.of(i);
                    }
                }

                return Stream.empty();
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return (Integer) stream.findFirst().orElse(-1);
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "head")
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

    @SquigglyFunctionMethod(aliases = "tail")
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

    public static Object flatMap(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> coreLambda.invoke(wrapper.get(i), i, wrapper.getValue()))
                        .flatMap(result -> CoreIterables.wrap(result).stream());
            }
        }.handle(value);
    }

    public static Object flatMap(Object value, Function function) {
        if (function == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().flatMap(value -> {
                    Object result = function.apply(value);
                    return CoreIterables.wrap(result).stream();
                });
            }
        }.handle(value);
    }


    public static Object map(Object value, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> coreLambda.invoke(wrapper.get(i), i, wrapper.getValue()));
            }
        }.handle(value);
    }

    public static Object map(Object value, Function function) {
        if (function == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().map((Function<Object, Object>) function);
            }
        }.handle(value);
    }

    public static Map groupBy(Object value, Function keyFunction) {
        return groupBy(value, keyFunction, Function.identity());
    }

    public static Map groupBy(Object value, Function keyFunction, Function valueFunction) {
        return groupBy(value, keyFunction, valueFunction, Function.identity());
    }

    public static Map groupBy(Object value, Function keyFunction, Function valueFunction, Function finisher) {
        return new BaseStreamingCollectionValueHandler<Map>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size()).mapToObj(i -> i);
            }

            @Override
            protected Map handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                Function wrappedKeyFunction = keyFunction instanceof CoreLambda
                        ? (index) -> ((CoreLambda) keyFunction).invoke(wrapper.get((Integer) index), index, wrapper.getValue())
                        : (index) -> keyFunction.apply(wrapper.get((Integer) index));

                Function wrappedValueFunction = valueFunction instanceof CoreLambda
                        ? (index) -> ((CoreLambda) valueFunction).invoke(wrapper.get((Integer) index), index, wrapper.getValue())
                        : (index) -> valueFunction.apply(wrapper.get((Integer) index));

                return (Map) stream.collect(Collectors.groupingBy(wrappedKeyFunction,
                        Collectors.mapping((Function<Object, Object>) wrappedValueFunction,
                                Collectors.collectingAndThen(
                                        Collectors.toList(),
                                        finisher))));
            }
        }.handle(value);
    }

    public static Map keyBy(Object value, Function keyFunction) {
        return keyBy(value, keyFunction, Function.identity());
    }


    public static Map keyBy(Object value, Function keyFunction, Function valueFunction) {
        return new BaseStreamingCollectionValueHandler<Map>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size()).mapToObj(i -> i);
            }

            @Override
            protected Map handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                Function wrappedKeyFunction = keyFunction instanceof CoreLambda
                        ? (index) -> ((CoreLambda) keyFunction).invoke(wrapper.get((Integer) index), index, wrapper.getValue())
                        : (index) -> keyFunction.apply(wrapper.get((Integer) index));

                Function wrappedValueFunction = valueFunction instanceof CoreLambda
                        ? (index) -> ((CoreLambda) valueFunction).invoke(wrapper.get((Integer) index), index, wrapper.getValue())
                        : (index) -> valueFunction.apply(wrapper.get((Integer) index));

                return (Map) stream.collect(Collectors.toMap(wrappedKeyFunction, wrappedValueFunction, (a, b) -> b));
            }
        }.handle(value);
    }

    public static Object reduce(Object value, Object initialValue, CoreLambda coreLambda) {
        if (coreLambda == null) {
            return initialValue;
        }

        return new BaseCollectionValueHandler<Object>(coreLambda) {
            @Override
            protected Object handleNull() {
                return initialValue;
            }

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                if (wrapper.isEmpty()) {
                    return initialValue;
                }

                Object accumulator = initialValue;

                for (int i = 0; i < wrapper.size(); i++) {
                    accumulator = coreLambda.invoke(accumulator, wrapper.get(i), i, wrapper.getValue());
                }

                return accumulator;
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

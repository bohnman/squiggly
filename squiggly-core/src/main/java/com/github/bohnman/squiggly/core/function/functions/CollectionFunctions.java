package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.core.function.value.BaseCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.value.BaseStreamingCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.value.CollectionReturningValueHandler;
import com.github.bohnman.squiggly.core.function.value.ValueHandler;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings("unchecked")
public class CollectionFunctions {
    private CollectionFunctions() {
    }

    public static boolean in(Object value, Object collection) {
        return new BaseStreamingCollectionValueHandler<Boolean>(collection) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream();
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.anyMatch(o -> Objects.equals(o, value));
            }
        }.handle(collection);
    }

    @SquigglyFunctionMethod(aliases = {"chunkBy", "partition", "partitionBy"})
    public static Object chunk(Object value, Number size) {
        int chunkSize = size == null ? 0 : Math.max(0, size.intValue());

        if (chunkSize == 0) {
            return value;
        }

        return new BaseCollectionValueHandler<Object>(size) {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                int wrapperSize = wrapper.size();

                if (wrapperSize == 0) {
                    return wrapper.stream();
                }

                int numEvenChunks = wrapperSize / chunkSize;
                int remainder = wrapperSize % chunkSize;
                int resultSize = numEvenChunks + (remainder == 0 ? 0 : 1);
                int lastChunkSize = remainder == 0 ? chunkSize : remainder;

                CoreIndexedIterableWrapper<Object, ?> result = wrapper.create(resultSize);

                for (int i = 0; i < resultSize - 1; i += chunkSize) {
                    CoreIndexedIterableWrapper<Object, ?> chunk = wrapper.create(chunkSize);

                    for (int j = 0; j < chunkSize; j++) {
                        chunk.set(j, wrapper.get(i + j));
                    }

                    result.set(i, chunk.getValue());
                }

                CoreIndexedIterableWrapper<Object, ?> lastChunk = wrapper.create(lastChunkSize);

                for (int i = 0; i < lastChunkSize; i++) {
                    lastChunk.set(i, wrapper.get(wrapperSize - lastChunkSize + i));
                }

                result.set(resultSize - 1, lastChunk.getValue());
                return result.getValue();
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = {"chunkBy", "partition", "partitionBy"})
    public static Object chunk(Object value, CoreLambda lambda) {

        return new CollectionReturningValueHandler(lambda) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                Function<Integer, Object> wrappedKeyFunction = (index) -> lambda.invoke(wrapper.get((Integer) index), index, wrapper.getValue());

                Map<Object, List<Object>> map = IntStream.range(0, wrapper.size())
                        .boxed()
                        .collect(Collectors.groupingBy(
                                wrappedKeyFunction,
                                LinkedHashMap::new,
                                Collectors.mapping(wrapper::get, Collectors.toList())));

                return map.values()
                        .stream()
                        .map(group -> wrapper.getValue() instanceof List ? group : wrapper.collect(group.stream()));


            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "every")
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
                List list = stream.collect(Collectors.toList());
                return list.isEmpty() ? null : list.get(list.size() - 1);
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
                List list = stream.collect(Collectors.toList());
                return list.isEmpty() ? -1 : (Integer) list.get(list.size() - 1);
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

    public static Map groupBy(Object value, CoreLambda keyFunction) {
        return groupBy(value, keyFunction, CoreLambda.identity());
    }

    public static Map groupBy(Object value, CoreLambda keyFunction, CoreLambda valueFunction) {
        return groupBy(value, keyFunction, valueFunction, CoreLambda.identity());
    }

    public static Map groupBy(Object value, CoreLambda keyFunction, CoreLambda valueFunction, CoreLambda finisher) {
        return new BaseStreamingCollectionValueHandler<Map>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size()).mapToObj(i -> i);
            }

            @Override
            protected Map handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                Function wrappedKeyFunction = (index) -> keyFunction.invoke(wrapper.get((Integer) index), index, wrapper.getValue());
                Function wrappedValueFunction = (index) -> valueFunction.invoke(wrapper.get((Integer) index), index, wrapper.getValue());
                Function wrappedFinisher = finisher;

                return (Map) stream.collect(Collectors.groupingBy(wrappedKeyFunction,
                        Collectors.mapping((Function<Object, Object>) wrappedValueFunction,
                                Collectors.collectingAndThen(
                                        Collectors.toList(),
                                        wrappedFinisher))));
            }
        }.handle(value);
    }

    public static Map keyBy(Object value, CoreLambda keyFunction) {
        return keyBy(value, keyFunction, CoreLambda.identity());
    }


    public static Map keyBy(Object value, CoreLambda keyFunction, CoreLambda valueFunction) {
        return new BaseStreamingCollectionValueHandler<Map>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size()).mapToObj(i -> i);
            }

            @Override
            protected Map handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                Function wrappedKeyFunction = (index) -> keyFunction.invoke(wrapper.get((Integer) index), index, wrapper.getValue());
                Function wrappedValueFunction = (index) -> valueFunction.invoke(wrapper.get((Integer) index), index, wrapper.getValue());
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

    @SquigglyFunctionMethod(aliases = {"average", "averageBy", "avgBy", "mean", "meanBy"})
    public static Number avg(Object value) {
        return avg(value, CoreLambda.identity());
    }

    @SquigglyFunctionMethod(aliases = {"average", "averageBy", "avgBy", "mean", "meanBy"})
    public static Number avg(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Number>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()));
            }

            @Override
            protected Number handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return NumberFunctions.cast(stream.map(CoreConversions::toNumber)
                        .filter(Objects::nonNull)
                        .mapToDouble(Number::doubleValue)
                        .average().orElse(0));
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = "countBy")
    public static Number count(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Number>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()));
            }

            @Override
            protected Number handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return NumberFunctions.cast(stream.map(CoreConversions::toNumber)
                        .filter(Objects::nonNull)
                        .count());
            }
        }.handle(value);
    }


    @SquigglyFunctionMethod(aliases = "sumBy")
    public static Number sum(Object value) {
        return sum(value, CoreLambda.identity());
    }

    @SquigglyFunctionMethod(aliases = "sumBy")
    public static Number sum(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Number>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()));
            }

            @Override
            protected Number handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return NumberFunctions.cast(stream.map(CoreConversions::toNumber)
                        .filter(Objects::nonNull)
                        .mapToDouble(Number::doubleValue)
                        .sum());
            }
        }.handle(value);
    }

    public static Object max(Object value) {
        return max(value, CoreLambda.identity());
    }

    public static Object max(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @SuppressWarnings("RedundantCast")
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> {
                            Object result = lambda.invoke(wrapper.get(i), i, wrapper.getValue());
                            return CorePair.of(result, toComparable(result));
                        })
                        .sorted((Comparator<CorePair<Object, Comparable>>) (o1, o2) -> -1 * o1.getRight().compareTo(o2.getRight()))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    public static Object maxBy(Object value) {
        return maxBy(value, CoreLambda.identity());
    }

    public static Object maxBy(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @SuppressWarnings("RedundantCast")
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CorePair.of(wrapper.get(i), toComparable(lambda.invoke(wrapper.get(i), i, wrapper.getValue()))))
                        .sorted((Comparator<CorePair<Object, Comparable>>) (o1, o2) -> -1 * o1.getRight().compareTo(o2.getRight()))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    public static Object minBy(Object value) {
        return minBy(value, CoreLambda.identity());
    }

    public static Object minBy(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @SuppressWarnings({"RedundantCast", "ComparatorCombinators"})
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CorePair.of(wrapper.get(i), toComparable(lambda.invoke(wrapper.get(i), i, wrapper.getValue()))))
                        .sorted((Comparator<CorePair<Object, Comparable>>) (o1, o2) -> o1.getRight().compareTo(o2.getRight()))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    public static Object min(Object value) {
        return min(value, CoreLambda.identity());
    }

    public static Object min(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @SuppressWarnings({"RedundantCast", "ComparatorCombinators"})
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> {
                            Object result = lambda.invoke(wrapper.get(i), i, wrapper.getValue());
                            return CorePair.of(result, toComparable(result));
                        })
                        .sorted((Comparator<CorePair<Object, Comparable>>) (o1, o2) -> o1.getRight().compareTo(o2.getRight()))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(aliases = {"differenceBy", "diff", "diffBy"})
    public static Object difference(Object value1, Object value2) {
        return difference(value1, value2, CoreLambda.identity());
    }

    @SquigglyFunctionMethod(aliases = {"differenceBy", "diff", "diffBy"})
    public static Object difference(Object value1, Object value2, CoreLambda lambda) {

        CoreIndexedIterableWrapper<Object, ?> wrapper1 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value1);

        CoreIndexedIterableWrapper<Object, ?> wrapper2 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value2);


        List<Object> mappedList2 = IntStream.range(0, wrapper2.size())
                .mapToObj(i -> lambda.invoke(wrapper2.get(i), i, wrapper2.getValue()))
                .collect(Collectors.toList());


        return wrapper1.collect(IntStream.range(0, wrapper1.size())
                .filter(i -> {
                    Object result = lambda.invoke(wrapper1.get(i), i, wrapper1.getValue());
                    return !mappedList2.contains(result);
                })
                .mapToObj(wrapper1::get));
    }



    @SquigglyFunctionMethod(aliases = {"intersectionBy"})
    public static Object intersection(Object value1, Object value2, CoreLambda lambda) {

        CoreIndexedIterableWrapper<Object, ?> wrapper1 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value1);

        CoreIndexedIterableWrapper<Object, ?> wrapper2 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value2);


        List<Object> mappedList2 = IntStream.range(0, wrapper2.size())
                .mapToObj(i -> lambda.invoke(wrapper2.get(i), i, wrapper2.getValue()))
                .collect(Collectors.toList());


        return wrapper1.collect(IntStream.range(0, wrapper1.size())
                .filter(i -> {
                    Object result = lambda.invoke(wrapper1.get(i), i, wrapper1.getValue());
                    return mappedList2.contains(result);
                })
                .mapToObj(wrapper1::get));
    }





    @SquigglyFunctionMethod(aliases = {"unionBy"})
    public static Object union(Object value1, Object value2) {
        return union(value1, value2, CoreLambda.identity());
    }

    @SquigglyFunctionMethod(aliases = {"unionBY"})
    public static Object union(Object value1, Object value2, CoreLambda lambda) {

        CoreIndexedIterableWrapper<Object, ?> wrapper1 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value1);

        CoreIndexedIterableWrapper<Object, ?> wrapper2 = new BaseCollectionValueHandler<CoreIndexedIterableWrapper<Object, ?>>() {
            protected CoreIndexedIterableWrapper<Object, ?> handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper;
            }
        }.handle(value2);

        Set<Object> seen = new HashSet<>();

        IntPredicate predicate = (i) -> {
            CoreIndexedIterableWrapper<Object, ?> wrapper = wrapper1;

            if (i >= wrapper1.size()) {
                i -= wrapper1.size();
                wrapper = wrapper2;
            }

            return seen.add(lambda.invoke(wrapper.get(i), i, wrapper.getValue()));
        };

        return wrapper1.collect(IntStream.range(0, wrapper1.size() + wrapper2.size())
                .filter(predicate)
                .mapToObj(i -> i >= wrapper1.size() ? wrapper2.get(i - wrapper1.size()) : wrapper1.get(i)));
    }

    @SquigglyFunctionMethod(aliases = {"uniq", "uniqueBy", "uniqBy"})
    public static Object unique(Object value) {
        return unique(value, CoreLambda.identity());
    }

    @SquigglyFunctionMethod(aliases = {"uniq", "uniqueBy", "uniqBy"})
    public static Object unique(Object value, CoreLambda lambda) {
        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .boxed()
                        .collect(Collectors.toMap(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()), wrapper::get, (a, b) -> a))
                        .values()
                        .stream();

            }
        }.handle(value);
    }

    @SquigglyFunctionMethod(env = SquigglyFunction.Environment.SECURE)
    public static int[] range(CoreIntRange range) {
        CoreIntRange exclusive = range.toExclusive();
        return range(exclusive.getStart(), exclusive.getEnd());
    }

    @SquigglyFunctionMethod(env = SquigglyFunction.Environment.SECURE)
    public static int[] range(CoreIntRange range, Number step) {
        CoreIntRange exclusive = range.toExclusive();
        return range(exclusive.getStart(), exclusive.getEnd(), step);
    }

    @SquigglyFunctionMethod(env = SquigglyFunction.Environment.SECURE)
    public static int[] range(Number end) {
        return range(0, end);
    }

    @SquigglyFunctionMethod(env = SquigglyFunction.Environment.SECURE)
    public static int[] range(Number start, Number end) {
        Number step =  (start != null && end != null && start.intValue() > end.intValue()) ? -1 : 1;
        return range(start, end, step);
    }


    @SquigglyFunctionMethod(env = SquigglyFunction.Environment.SECURE)
    public static int[] range(Number start, Number end, Number step) {
        if (end == null || step == null) {
            return new int[0];
        }

        if (start == null) {
            start = 0;
        }

        int startInt = start.intValue();
        int endInt = end.intValue();
        int stepInt = step.intValue();

        if (stepInt == 0) {
            return new int[0];
        }

        int length = (int) Math.max(Math.ceil(((double) (endInt - startInt) / (double) stepInt)), 0);

        int[] range = new int[length];

        for (int i = 0; i < length; i++) {
            range[i] = startInt;
            startInt += stepInt;
        }

        return range;
    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(range(0, -5)));
    }

    private static Comparable toComparable(Object value) {
        if (value instanceof Comparable) {
            return (Comparable) value;
        }

        return o -> CoreObjects.firstNonNull(CoreObjects.compare(value, o), -1);

    }
}

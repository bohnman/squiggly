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
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.function.annotations.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.core.function.valuehandlers.BaseCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.valuehandlers.BaseStreamingCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.valuehandlers.CollectionReturningValueHandler;
import com.github.bohnman.squiggly.core.function.ValueHandler;

import java.util.*;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Collection-type functions.
 */
@SuppressWarnings("unchecked")
public class CollectionFunctions {

    private static final int MAX_RANGE_LENGTH = 100;


    private CollectionFunctions() {
    }

    /**
     * Returns true if lambda returns true for all items in value, otherwise false.
     *
     * @param value  collection-like value
     * @param lambda the test logic
     * @return true/false
     */
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

    /**
     * Returns true if lambda returns true for any items in value, otherwise false.
     *
     * @param value  collection-like value
     * @param lambda the test logic
     * @return true/false
     */
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

    /**
     * Returns the mean of the items in value.
     *
     * @param value collection-like value
     * @return average
     */
    @SquigglyFunctionMethod(aliases = {"average", "mean"})
    public static Number avg(Object value) {
        return avgBy(value, CoreLambda.identity());
    }

    /**
     * Returns the mean of the items in value.  The lambda param is used as a mapper function to retrieve the number.
     *
     * @param value  collection-like value
     * @param lambda the test logic
     * @return average
     */
    @SquigglyFunctionMethod(aliases = {"averageBy", "meanBy"})
    public static Number avgBy(Object value, CoreLambda lambda) {
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

    /**
     * Partition a collection into chunks of size.
     *
     * @param value collection like object
     * @param size  number of chunks
     * @return collection
     */
    @SquigglyFunctionMethod(aliases = {"partition"})
    public static Object chunk(Object value, Number size) {
        int chunkSize = size == null ? 0 : Math.max(0, size.intValue());

        if (chunkSize == 0) {
            return value;
        }

        return new BaseCollectionValueHandler<Object>(size) {

            @SuppressWarnings("RedundantOperationOnEmptyContainer")
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

    /**
     * Partition a collection into chunks using lambda as a grouping function.
     *
     * @param value  collection like object
     * @param lambda lambda used to group the function
     * @return collection
     */
    @SquigglyFunctionMethod(aliases = {"partitionBy"})
    public static Object chunkBy(Object value, CoreLambda lambda) {

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

    /**
     * Count the number of items in value using lambda as a predicate.
     *
     * @param value  collection like value
     * @param lambda predicate function
     * @return count
     */
    public static Number countBy(Object value, CoreLambda lambda) {
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

    /**
     * Return the items in value1 and that are not in value2 and vice versa.
     *
     * @param value1 collection 1
     * @param value2 collection 2
     * @return items
     */
    @SquigglyFunctionMethod(aliases = {"diff"})
    public static Object difference(Object value1, Object value2) {
        return differenceBy(value1, value2, CoreLambda.identity());
    }

    /**
     * Return the items in value1 and that are not in value2 and vice versa, using lambda as a mapping function.
     *
     * @param value1 collection 1
     * @param value2 collection 2
     * @return items
     */
    @SquigglyFunctionMethod(aliases = {"diffBy"})
    public static Object differenceBy(Object value1, Object value2, CoreLambda lambda) {

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

    /**
     * Retain all items in value using lambda as a predicate.
     *
     * @param value  collection like object
     * @param lambda predicate
     * @return collection
     */
    public static Object filter(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

        }.handle(value);
    }

    /**
     * Find the first item in value matching the lambda predicate or null if not found.
     *
     * @param value  collection like object
     * @param lambda lambda predicate
     * @return found item or null
     */
    public static Object findFirst(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return null;
        }

        return new BaseStreamingCollectionValueHandler<Object>(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    /**
     * Find the index of the first item in value matching the lambda predicate or -1 if not found.
     *
     * @param value  collection like object
     * @param lambda lambda predicate
     * @return found index or -1
     */
    public static int findIndex(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(i -> i);
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return (Integer) stream.findFirst().orElse(-1);
            }
        }.handle(value);
    }

    /**
     * Find the last item in value matching the lambda predicate or null if not found
     *
     * @param value  collection like object
     * @param lambda lambda predicate
     * @return found item or null
     */
    public static Object findLast(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return null;
        }

        return new BaseStreamingCollectionValueHandler<Object>(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                List list = stream.collect(Collectors.toList());
                return list.isEmpty() ? null : list.get(list.size() - 1);
            }
        }.handle(value);
    }

    /**
     * Find the index of the last item in value matching the lambda predicate or -1 if not found.
     *
     * @param value  collection like object
     * @param lambda lambda predicate
     * @return found index or -1
     */
    public static int findLastIndex(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return -1;
        }

        return new BaseStreamingCollectionValueHandler<Integer>(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(i -> i);
            }

            @Override
            protected Integer handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                List list = stream.collect(Collectors.toList());
                return list.isEmpty() ? -1 : (Integer) list.get(list.size() - 1);
            }
        }.handle(value);
    }

    /**
     * Return the first item in the collection or null if empty.
     *
     * @param value collection type object
     * @return item or null
     */
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

    /**
     * Take a collection of collections and convert it to a collection of items, using lambda as a mapper function.
     *
     * @param value  collection like object
     * @param lambda lambda mapper
     * @return flattened collection
     */
    public static Object flatMap(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()))
                        .flatMap(result -> CoreIterables.wrap(result).stream());
            }
        }.handle(value);
    }

    /**
     * Group by items in value by the value returned by keyFunction.
     *
     * @param value       collection-like object
     * @param keyFunction key mapper
     * @return map
     */
    public static Map groupBy(Object value, CoreLambda keyFunction) {
        return groupBy(value, keyFunction, CoreLambda.identity());
    }

    /**
     * Group by items in value by the value returned by keyFunction, converting values using valueFunction.
     *
     * @param value         collection-like object
     * @param keyFunction   key mapper
     * @param valueFunction value converter
     * @return map
     */
    public static Map groupBy(Object value, CoreLambda keyFunction, CoreLambda valueFunction) {
        return groupBy(value, keyFunction, valueFunction, CoreLambda.identity());
    }

    /**
     * Group by items in value by the value returned by keyFunction, converting values using valueFunction.
     *
     * @param value         collection-like object
     * @param keyFunction   key mapper
     * @param valueFunction value converter
     * @param finisher      function to apply on the collection for each entry
     * @return map
     */
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

    /**
     * Determines if value is in collection.
     *
     * @param value      any
     * @param collection collection-like object
     * @return true if contains
     */
    public static boolean in(Object value, Object collection) {
        return new BaseStreamingCollectionValueHandler<Boolean>(collection) {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream();
            }

            @Override
            protected Boolean handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.anyMatch(o -> CoreObjects.equals(o, value));
            }
        }.handle(collection);
    }

    /**
     * Determines if value is in collection.
     *
     * @param value      any
     * @param candidates candidate objects
     * @return true if contains
     */
    public static boolean in(Object value, Object... candidates) {
        for (Object candidate : candidates) {
            if (CoreObjects.equals(value, candidate)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Return all items contained in both value1 and value2.
     *
     * @param value1 collection-like object
     * @param value2 collection-like object
     * @return collection
     */
    public static Object intersection(Object value1, Object value2) {
        return intersectionBy(value1, value2, CoreLambda.identity());
    }

    /**
     * Return all items contained in both value1 and value2 using lambda as a mapper.
     *
     * @param value1 collection-like object
     * @param value2 collection-like object
     * @param lambda mapper function
     * @return collection
     */
    public static Object intersectionBy(Object value1, Object value2, CoreLambda lambda) {

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

    /**
     * Create a map using keyFunction to extract the key and whose value is the last item matched on that key.
     *
     * @param value       collection-like object
     * @param keyFunction key mapper
     * @return map
     */
    public static Map keyBy(Object value, CoreLambda keyFunction) {
        return keyBy(value, keyFunction, CoreLambda.identity());
    }

    /**
     * Create a map using keyFunction to extract the key and whose value is the last item matched on that key, applying
     * the valueMapper to convert the entry value.
     *
     * @param value       collection-like object
     * @param keyFunction key mapper
     * @return map
     */
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

    /**
     * Return the first item in the collection or null if empty.
     *
     * @param value collection type object
     * @return item or null
     */
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

    /**
     * Convert the items in the collection using lambda as a mapper function.
     *
     * @param value  collection-like object
     * @param lambda mapper function
     * @return collection
     */
    public static Object map(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> lambda.invoke(wrapper.get(i), i, wrapper.getValue()));
            }
        }.handle(value);
    }

    /**
     * Return the max item in value or null if collection is empty.
     *
     * @param value collection-like object
     * @return max or null
     */
    public static Object max(Object value) {
        return maxBy(value, CoreLambda.identity());
    }

    /**
     * Return the max item in value or null if collection is empty, using the lambda as a mapper function
     *
     * @param value collection-like object
     * @return max or null
     */
    public static Object maxBy(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> {
                            Object result = lambda.invoke(wrapper.get(i), i, wrapper.getValue());
                            return CorePair.of(wrapper.get(i), result);
                        })
                        .sorted((o1, o2) -> -1 * CoreObjects.compare(o1.getRight(), o2.getRight(), -1))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    /**
     * Return the min item in value or null if collection is empty.
     *
     * @param value collection-like object
     * @return min or null
     */
    public static Object min(Object value) {
        return minBy(value, CoreLambda.identity());
    }

    /**
     * Return the min item in value or null if collection is empty, using the lambda as a mapper function
     *
     * @param value collection-like object
     * @return min or null
     */
    public static Object minBy(Object value, CoreLambda lambda) {
        return new BaseStreamingCollectionValueHandler<Object>() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> {
                            Object result = lambda.invoke(wrapper.get(i), i, wrapper.getValue());
                            return CorePair.of(wrapper.get(i), result);
                        })
                        .sorted((o1, o2) -> CoreObjects.compare(o1.getRight(), o2.getRight(), -1))
                        .map(CorePair::getLeft);
            }

            @Override
            protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
                return stream.findFirst().orElse(null);
            }
        }.handle(value);
    }

    /**
     * Return true if no items in value match the lambda predicate.
     *
     * @param value  collection-like object
     * @param lambda lambda predicate
     * @return true if no item matches
     */
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

    /**
     * Create a range of integers constrained by range.
     *
     * @param range core int range
     * @return range of int
     */
    public static int[] range(CoreIntRange range) {
        CoreIntRange exclusive = range.toExclusive();
        return range(exclusive.getStart(), exclusive.getEnd());
    }

    /**
     * Create a range of integers constrained by range, using step is an increment value.
     *
     * @param range core int range
     * @param step  number to increment by
     * @return range of int
     */
    public static int[] range(CoreIntRange range, Number step) {
        CoreIntRange exclusive = range.toExclusive();
        return range(exclusive.getStart(), exclusive.getEnd(), step);
    }

    /**
     * Create a range of integers constrained starting with 0 and ending with end - 1.
     *
     * @param end the end number exclusive
     * @return range of int
     */
    public static int[] range(Number end) {
        return range(0, end);
    }

    /**
     * Create a range of integers constrained starting with start and ending with end - 1.
     *
     * @param start the start number inclusive
     * @param end   the end number exclusive
     * @return range of int
     */
    public static int[] range(Number start, Number end) {
        Number step = (start != null && end != null && start.intValue() > end.intValue()) ? -1 : 1;
        return range(start, end, step);
    }

    /**
     * Create a range of integers constrained starting with start and ending with end - 1, incrementing by step.
     *
     * @param start the start number inclusive
     * @param end   the end number exclusive
     * @param step  the number to increment by
     * @return range of int
     */
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
        length = Math.min(length, MAX_RANGE_LENGTH);

        int[] range = new int[length];

        for (int i = 0; i < length; i++) {
            range[i] = startInt;
            startInt += stepInt;
        }

        return range;
    }

    /**
     * Convert the items in value into a single item, using an initial value and a reducer function.
     *
     * @param value        collection-like value
     * @param initialValue initial value
     * @param lambda       reducer function
     * @return item or null
     */
    public static Object reduce(Object value, Object initialValue, CoreLambda lambda) {
        if (lambda == null) {
            return initialValue;
        }

        return new BaseCollectionValueHandler<Object>(lambda) {
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
                    accumulator = lambda.invoke(accumulator, wrapper.get(i), i, wrapper.getValue());
                }

                return accumulator;
            }
        }.handle(value);
    }

    /**
     * Retrieve all items in value that doesn't match lambda.
     *
     * @param value  collection-like object
     * @param lambda predicate function
     * @return items not matching predicate
     */
    public static Object reject(Object value, CoreLambda lambda) {
        if (lambda == null) {
            return Collections.emptyList();
        }

        return new CollectionReturningValueHandler(lambda) {

            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> !CoreConversions.toBoolean(lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .mapToObj(wrapper::get);
            }

        }.handle(value);
    }

    /**
     * Sum the items in value.
     *
     * @param value collection-like object
     * @return sum
     */
    public static Number sum(Object value) {
        return sumBy(value, CoreLambda.identity());
    }

    /**
     * Sum the items in value using lambda as a mapping function.
     *
     * @param value  collection-like object
     * @param lambda mapping function
     * @return sum
     */
    public static Number sumBy(Object value, CoreLambda lambda) {
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

    /**
     * Convert the value to an array.
     *
     * @param value collection-like object
     * @return array
     */
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

    /**
     * Convert value to a list.
     *
     * @param value collection-like object
     * @return list
     */
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

    /**
     * Convert the value to a map.
     *
     * @param squiggly base squiggly
     * @param value collection-like object
     * @return map
     */
    public static Map<?, ?> toMap(BaseSquiggly squiggly, Object value) {
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
                return MixedFunctions.toMap(squiggly, value);
            }
        }.handle(value);
    }

    /**
     * Return all items in value1 and value2, removing duplicates.
     *
     * @param value1 collection-like object
     * @param value2 collection-like object
     * @return unition
     */
    public static Object union(Object value1, Object value2) {
        return unionBy(value1, value2, CoreLambda.identity());
    }

    /**
     * Return all items in value1 and value2, removing duplicates using lambda as a mapper function.
     *
     * @param value1 collection-like object
     * @param value2 collection-like object
     * @param lambda mapper function
     * @return unition
     */
    public static Object unionBy(Object value1, Object value2, CoreLambda lambda) {

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

    /**
     * Retrive all the unique items in value.
     *
     * @param value collection-like object
     * @return unique items
     */
    @SquigglyFunctionMethod(aliases = {"distinct", "uniq"})
    public static Object unique(Object value) {
        return uniqueBy(value, CoreLambda.identity());
    }

    /**
     * Retrive all the unique items in value, using lambda as a mapper function.
     *
     * @param value  collection-like object
     * @param lambda mapper function
     * @return unique items
     */
    @SquigglyFunctionMethod(aliases = {"distinctBy", "uniqBy"})
    public static Object uniqueBy(Object value, CoreLambda lambda) {
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
}

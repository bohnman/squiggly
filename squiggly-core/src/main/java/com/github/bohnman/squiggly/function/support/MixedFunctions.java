package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.*;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.engine.SquigglyEngine;
import com.github.bohnman.squiggly.function.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.function.ValueHandler;

import javax.annotation.Nullable;
import java.beans.PropertyDescriptor;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

/**
 * Functions that accept mixed types as arguments.
 */
@SuppressWarnings("unchecked")
public class MixedFunctions {

    public MixedFunctions() {
    }

    /**
     * Concatenate strings and collections together.
     *
     * @param o1 string or collection
     * @param o2 string or collection
     * @return concatenated value
     */
    public static Object concat(Object o1, Object o2) {
        if (o1 instanceof String || o2 instanceof String) {
            return CoreStrings.defaultIfEmpty(CoreConversions.safeToString(o1), "") + CoreStrings.defaultIfEmpty(CoreConversions.safeToString(o2), "");
        }

        if (o1 == null || o2 == null) {
            return null;
        }

        CoreIndexedIterableWrapper<Object, ?> w1 = CoreIterables.wrap(o1);
        CoreIndexedIterableWrapper<Object, ?> w2 = CoreIterables.wrap(o2);
        return w1.collect(Stream.concat(w1.stream(), w2.stream()));
    }

    /**
     * See if the haystack object contains a needle.
     *
     * @param haystack collection/string
     * @param needle   item
     * @return true/false
     */
    public static boolean contains(Object haystack, Object needle) {
        return new ValueHandler<Boolean>() {

            @Override
            protected Boolean handleNull() {
                return false;
            }

            @Override
            protected Boolean handleString(String string) {
                return string.contains(CoreConversions.safeToString(needle));
            }

            @Override
            protected Boolean handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().anyMatch(candidate -> CoreObjects.equals(candidate, needle));
            }

            @Override
            protected Boolean handleObject(Object value) {
                return false;
            }
        }.handle(haystack);
    }

    /**
     * Retrieve the item at key.  For strings/collections, the key should be the index.  For maps, it should be the
     * map key.  For pojos, this is the property of the bean.
     *
     * @param squiggly base squiggly
     * @param value    an object
     * @param key      the key
     * @return value at the key or null
     */
    public static Object get(SquigglyEngine squiggly, Object value, Object key) {
        if (key == null) {
            return null;
        }

        return new ValueHandler<Object>(key) {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper wrapper) {
                Integer actualIndex = normalizeIndex(wrapper.size(), key);
                if (actualIndex == null) return null;
                return wrapper.get(actualIndex);
            }

            @Override
            protected Object handleMap(Map<Object, Object> map) {
                return map.get(key);
            }

            @Override
            protected Object handleString(String string) {
                Integer actualIndex = normalizeIndex(string.length(), key);
                if (actualIndex == null) return null;
                return Character.toString(string.charAt(actualIndex));
            }

            @Override
            protected Object handleObject(Object value) {
                return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .filter(pd -> pd.getName().equals(CoreConversions.safeToString(key)))
                        .filter(pd -> squiggly.getFunctionSecurity().isPropertyViewable(pd.getName(), value.getClass()))
                        .map(pd -> CoreMethods.invoke(pd.getReadMethod(), value))
                        .findFirst()
                        .orElse(null);
            }
        }.handle(value);

    }

    /**
     * Determines if the object has the key.
     *
     * @param squiggly squiggly object
     * @param value    string/map/collection/pojo
     * @param key      map key or index
     * @return true if has
     */
    public static boolean has(SquigglyEngine squiggly, Object value, Object key) {
        if (value == null || key == null) {
            return false;
        }

        return new ValueHandler<Boolean>(key) {

            @Override
            protected Boolean handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                int size = wrapper.size();
                Integer actualIndex = normalizeIndex(size, key);
                if (actualIndex == null) return false;
                return actualIndex >= 0 && actualIndex < size;
            }

            @Override
            protected Boolean handleMap(Map<Object, Object> map) {
                return map.containsKey(key);
            }

            @Override
            protected Boolean handleString(String string) {
                int size = string.length();
                Integer actualIndex = normalizeIndex(size, key);
                if (actualIndex == null) return false;
                return actualIndex >= 0 && actualIndex < size;
            }

            @Override
            protected Boolean handleObject(Object value) {
                return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .anyMatch(pd -> pd.getName().equals(CoreConversions.safeToString(key))
                                && squiggly.getFunctionSecurity().isPropertyViewable(pd.getName(), value.getClass()));
            }
        }.handle(value);
    }

    /**
     * Returns index of the first occurrence item in a collection/string.
     *
     * @param haystack collection/string
     * @param needle   item to search
     * @return index or -1 if not found
     */
    public static int indexOf(Object haystack, Object needle) {
        if (haystack == null || needle == null) {
            return -1;
        }

        return new ValueHandler<Integer>(needle) {
            @Override
            protected Integer handleString(String string) {
                return string.indexOf("" + CoreConversions.safeToString(needle));
            }

            @Override
            protected Integer handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .filter(i -> CoreObjects.equals(wrapper.get(i), needle))
                        .findFirst()
                        .orElse(-1);

            }

            @Override
            protected Integer handleObject(Object value) {
                return -1;
            }
        }.handle(haystack);
    }

    /**
     * Retrieve all the keys of the value.  For collections, this will be all of the index.  For maps, this will be
     * all the map keys.  For pojos, this will be all the properties.
     *
     * @param squiggly squiggly object
     * @param value    collection/map/pojo
     * @return keys
     */
    public static Object keys(SquigglyEngine squiggly, Object value) {
        return new BaseCollectionValueHandler<Object>() {
            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                int size = wrapper.size();
                return IntStream.range(0, size)
                        .boxed()
                        .collect(toList());
            }

            @Override
            protected Object handleMap(Map<Object, Object> map) {
                return ((Map) value).keySet();
            }

            @Override
            protected Object handleObject(Object value) {
                return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .map(PropertyDescriptor::getName)
                        .filter(name -> squiggly.getFunctionSecurity().isPropertyViewable(name, value.getClass()))
                        .collect(toList());
            }
        }.handle(value);
    }

    /**
     * Returns index of the last occurrence item in a collection/string.
     *
     * @param haystack collection/string
     * @param needle   item to search
     * @return index or -1 if not found
     */
    public static int lastIndexOf(Object haystack, Object needle) {
        if (haystack == null || needle == null) {
            return -1;
        }

        return new ValueHandler<Integer>(needle) {
            @Override
            protected Integer handleString(String string) {
                return string.lastIndexOf("" + CoreConversions.safeToString(needle));
            }

            @Override
            protected Integer handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                List<Integer> matches = IntStream.range(0, wrapper.size())
                        .filter(i -> CoreObjects.equals(wrapper.get(i), needle))
                        .boxed()
                        .collect(Collectors.toList());

                return matches.isEmpty() ? -1 : matches.get(matches.size() - 1);

            }

            @Override
            protected Integer handleObject(Object value) {
                return -1;
            }
        }.handle(haystack);
    }

    /**
     * Return the first/last max items from the value.  If max is negative, the last abs(max) items are returned,
     * otherwise the first max items are returned.
     *
     * @param value string/collection
     * @param max   limit
     * @return items
     */
    public static Object limit(Object value, Number max) {
        if (max == null) {
            return value;
        }

        int limit = max.intValue();

        if (limit < 0) {
            return slice(value, limit);
        } else {
            return slice(value, 0, limit);
        }
    }

    /**
     * Determines whether the value contains the given pattern.
     *
     * @param squiggly squiggly object
     * @param value    collection/string/pojo
     * @param pattern  regex
     * @return true if match
     */
    public static boolean match(SquigglyEngine squiggly, Object value, Pattern pattern) {
        return new ValueHandler<Boolean>(pattern) {
            @Override
            protected Boolean handleNull() {
                return false;
            }

            @Override
            protected Boolean handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().anyMatch(e -> match(squiggly, e, pattern));
            }

            @Override
            protected Boolean handleString(String string) {
                return pattern.matcher(string).find();
            }

            @Override
            protected Boolean handleObject(Object value) {
                return handleList(CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .map(PropertyDescriptor::getName)
                        .filter(name -> squiggly.getFunctionSecurity().isPropertyViewable(name, value.getClass()))
                        .collect(toList()));
            }
        }.handle(value);
    }

    /**
     * Determines whether the value does not contain the given pattern.
     *
     * @param squiggly squiggly object
     * @param value    collection/string/pojo
     * @param pattern  regex
     * @return true if no match
     */
    @SquigglyFunctionMethod(aliases = "nmatch")
    public static boolean notMatch(SquigglyEngine squiggly, Object value, Pattern pattern) {
        return !match(squiggly, value, pattern);
    }

    /**
     * Retrieve the specified keys from the value.  If the key is an int range, it will expand the range to all the
     * keys within that range.
     *
     * @param squiggly squiggly object
     * @param value    collection/string/pojo
     * @param keys     int/string/intrange
     * @return all items that match the keys
     */
    public static Object pick(SquigglyEngine squiggly, Object value, Object... keys) {

        if (keys.length == 0) {
            return value;
        }

        return new ValueHandler<Object>() {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), keys);
                if (actualIndexes.isEmpty()) return wrapper.create(0);
                CoreIndexedIterableWrapper newWrapper = wrapper.create(actualIndexes.size());
                for (int i = 0; i < actualIndexes.size(); i++) {
                    newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
                }
                return newWrapper.getValue();
            }

            @Override
            protected Object handleString(String string) {
                List<Integer> actualIndexes = normalizeIndexes(string.length(), keys);
                if (actualIndexes.isEmpty()) return "";
                StringBuilder builder = new StringBuilder(actualIndexes.size());
                for (Integer actualIndex : actualIndexes) {
                    builder.append(string.charAt(actualIndex));
                }

                return builder.toString();
            }

            @Override
            protected Object handleObject(Object value) {
                Map<String, Object> map = toMap(squiggly, value);
                List<Object> keyList = Arrays.asList(keys);

                return map.entrySet()
                        .stream()
                        .filter(entry -> keyList.contains(entry.getKey()))
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            }
        }.handle(value);

    }

    /**
     * Retrieve all items except those associated with the specified keys.  If the key is an int range,
     * it will expand the range to all the keys within that range.
     *
     * @param squiggly  squiggly object
     * @param value collection/string/pojo
     * @param keys  int/string/intrange
     * @return all items that do not match the keys
     */
    public static Object pickExcept(SquigglyEngine squiggly, Object value, Object... keys) {
        if (keys.length == 0) {
            return value;
        }

        return new ValueHandler<Object>() {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), keys);
                if (actualIndexes.isEmpty()) return wrapper.create(0);
                CoreIndexedIterableWrapper<Object, ?> newWrapper = wrapper.create(Math.max(0, wrapper.size() - actualIndexes.size()));
                int newIdx = 0;

                for (int i = 0; i < wrapper.size(); i++) {
                    if (!actualIndexes.contains(i)) {
                        newWrapper.set(newIdx++, wrapper.get(i));
                    }
                }

                for (int i = 0; i < actualIndexes.size(); i++) {
                    newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
                }
                return newWrapper.getValue();
            }

            @Override
            protected Object handleObject(Object value) {
                Map<String, Object> map = toMap(squiggly, value);
                List<Object> keyList = Arrays.asList(keys);

                return map.entrySet()
                        .stream()
                        .filter(entry -> !keyList.contains(entry.getKey()))
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            }

            @Override
            protected Object handleString(String string) {
                List<Integer> actualIndexes = normalizeIndexes(string.length(), keys);
                if (actualIndexes.isEmpty()) return "";
                StringBuilder builder = new StringBuilder(actualIndexes.size());
                char[] chars = string.toCharArray();
                for (int i = 0; i < chars.length; i++) {
                    char ch = chars[i];
                    if (!actualIndexes.contains(i)) {
                        builder.append(ch);
                    }
                }
                return builder.toString();
            }
        }.handle(value);

    }

    /**
     * Reverse the collection/string.
     *
     * @param value collection/string
     * @return reversed value
     */
    public static Object reverse(Object value) {
        return new ValueHandler<Object>() {

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                return CoreArrays.wrap(value).reverse().getValue();
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                List<Object> list = iterable instanceof List ? new ArrayList<>((List) iterable) : CoreLists.of(iterable);
                Collections.reverse(list);
                return list;
            }

            @Override
            protected Object handleObject(Object value) {
                return value;
            }

            @Override
            protected Object handleString(String string) {
                return CoreStrings.reverse((String) value);
            }
        }.handle(value);
    }

    /**
     * Return the size of the collection/map/string/pojo.
     *
     * @param squiggly base squiggly
     * @param value collection/map/string/pojo
     * @return size
     */
    @SquigglyFunctionMethod(aliases = {"length", "count", "countBy"})
    public static int size(SquigglyEngine squiggly, Object value) {
        return new ValueHandler<Integer>() {
            @Override
            protected Integer handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper.size();
            }

            @Override
            protected Integer handleIterable(Iterable<Object> iterable) {
                return CoreIterables.size(iterable);
            }

            @Override
            protected Integer handleNull() {
                return 0;
            }

            @Override
            protected Integer handleString(String string) {
                return string.length();
            }

            @Override
            protected Integer handleMap(Map<Object, Object> map) {
                return map.size();
            }

            @Override
            protected Integer handleObject(Object value) {
                return (int) CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .filter(pd -> squiggly.getFunctionSecurity().isPropertyViewable(pd.getName(), value.getClass()))
                        .count();
            }
        }.handle(value);
    }

    /**
     * Retrieve a subset of items in value.
     *
     * @param value collection/string
     * @param range int range
     * @return slice
     */
    public static Object slice(Object value, CoreIntRange range) {
        if (range == null) {
            return value;
        }

        range = range.toExclusive();

        Integer start = range.getStart();

        if (start == null) {
            return slice(value, 0, 0);
        }

        if (range.getEnd() == null) {
            return slice(value, start);
        }

        return slice(value, start, range.getEnd());
    }

    /**
     * Retrieve a subset of items in value, starting with startIndex until the end.
     *
     * @param value      collection/string
     * @param startIndex inclusive start
     * @return slice
     */
    public static Object slice(Object value, Number startIndex) {
        if (startIndex == null) {
            return value;
        }

        int start = startIndex.intValue();

        return new BaseCollectionValueHandler<Object>(start) {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                int len = wrapper.size();
                int realStart = CoreArrays.normalizeIndex(start, len);
                int realEnd = len;
                return (realStart >= realEnd) ? wrapper.create(0).getValue() : wrapper.slice(realStart).getValue();
            }

            @Override
            protected Object handleString(String string) {
                return CoreStrings.substring(string, start);
            }
        }.handle(value);
    }

    /**
     * Retrieve a subset of items in value, starting with startIndex until endIndex.
     *
     * @param value      collection/string
     * @param startIndex inclusive start
     * @param endIndex   exclusive start
     * @return slice
     */
    public static Object slice(Object value, Number startIndex, Number endIndex) {
        if (startIndex == null || endIndex == null) {
            return value;
        }

        int start = startIndex.intValue();
        int end = endIndex.intValue();

        return new BaseCollectionValueHandler<Object>(start, end) {
            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                int len = wrapper.size();
                int realStart = CoreArrays.normalizeIndex(start, len);
                int realEnd = CoreArrays.normalizeIndex(end, len);
                return (realStart >= realEnd) ? wrapper.create(0).getValue() : wrapper.slice(realStart, realEnd).getValue();
            }

            @Override
            protected Object handleString(String string) {
                return CoreStrings.substring(string, start, end);
            }
        }.handle(value);
    }

    /**
     * Sort the value by the specified properties.
     *
     * @param value      collection
     * @param properties list of properties
     * @return sorted collection
     */
    public static Object sort(Object value, CoreProperty... properties) {
        List<OrderBy> orderBys = Arrays.stream(properties)
                .map(PropertyOrderBy::new)
                .collect(toList());

        return new ValueHandler<Object>((Object[]) properties) {

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                List list = wrapper.stream()
                        .map(item -> newComparable(item, orderBys))
                        .sorted()
                        .map(OrderByComparable::getOriginalValue)
                        .collect(toList());

                CoreIndexedIterableWrapper<Object, ?> newWrapper = wrapper.create(list.size());

                for (int i = 0; i < list.size(); i++) {
                    newWrapper.set(i, list.get(i));
                }

                return newWrapper.getValue();
            }

            @Override
            protected Object handleObject(Object value) {
                return value;
            }
        }.handle(value);
    }

    /**
     * Sort items in value using a mapping function to retrieve the sort key.
     *
     * @param value  collection like object
     * @param lambda mapping function
     * @return sorted items
     */
    public static Object sortBy(Object value, CoreLambda lambda) {
        return new CollectionReturningValueHandler() {
            @Override
            protected Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return IntStream.range(0, wrapper.size())
                        .mapToObj(i -> CorePair.of(wrapper.get(i), lambda.invoke(wrapper.get(i), i, wrapper.getValue())))
                        .sorted((Comparator<? super CorePair<Object, Object>>) (o1, o2) -> CoreObjects.compare(o1.getRight(), o2.getRight(), -1))
                        .map(CorePair::getLeft);
            }
        };
    }

    /**
     * Retrieve all the values of a collection/map/pojo.
     *
     * @param squiggly squiggly object
     * @param value collection/map/pojo
     * @return values
     */
    public static Object values(SquigglyEngine squiggly, Object value) {
        return new BaseCollectionValueHandler<Object>() {
            @Override
            protected Object handleArray(Object array) {
                return array;
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                return iterable;
            }

            @Override
            protected Object handleObject(Object value) {
                return CoreLists.of(toMap(squiggly, value).values());
            }

            @Override
            protected Object handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return handleObject(wrapper.getValue());
            }
        }.handle(value);
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> toMap(SquigglyEngine squiggly, Object value) {
        if (value == null || value instanceof String) {
            return Collections.emptyMap();
        }

        if (value instanceof Map) {
            return (Map) value;
        }

        try {
            return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                    .filter(pd -> squiggly.getFunctionSecurity().isPropertyViewable(pd.getName(), value.getClass()))
                    .collect(Collectors.toMap(PropertyDescriptor::getName,
                            pd -> CoreMethods.invoke(pd.getReadMethod(), value)));
        } catch (Exception e) {
            return Collections.emptyMap();
        }
    }


    private static OrderByComparable newComparable(Object value, List<OrderBy> orderBys) {
        return new OrderByComparable(value, orderBys);
    }

    private static Integer normalizeIndex(int len, Object index) {
        if (index instanceof String) {
            try {
                index = Double.parseDouble((String) index);
            } catch (NumberFormatException e) {
                return null;
            }
        }

        if (index instanceof Number) {
            int actualIndex = CoreArrays.normalizeIndex(((Number) index).intValue(), len, -1, len);
            return actualIndex < 0 ? null : actualIndex;
        }

        return null;
    }

    private static List<Integer> normalizeIndexes(int len, Object... indexes) {
        return Stream.of(indexes)
                .flatMap(index -> {

                    if (index instanceof CoreIntRange) {
                        CoreIntRange range = (CoreIntRange) index;
                        int start = CoreArrays.normalizeIndex(CoreObjects.firstNonNull(range.getStart(), 0), len);
                        int end = CoreArrays.normalizeIndex(CoreObjects.firstNonNull(range.getEnd(), len), len);
                        return (start >= end) ? Stream.empty() : IntStream.range(start, end).boxed();
                    }

                    Integer normalized = normalizeIndex(len, index);
                    return normalized == null ? Stream.empty() : Stream.of(normalized);
                })
                .distinct()
                .collect(toList());
    }

    private static class OrderByComparable implements Comparable<Object> {

        private final Object originalValue;
        private final List<OrderBy> orderBys;
        private final Map<OrderBy, Comparable> orderByCache = new IdentityHashMap<>();

        public OrderByComparable(Object originalValue, List<OrderBy> orderBys) {
            this.originalValue = originalValue;
            this.orderBys = orderBys;
        }

        public Object getOriginalValue() {
            return originalValue;
        }

        @Override
        public int compareTo(@Nullable Object o) {
            if (o == null) return 1;

            OrderByComparable other = (OrderByComparable) o;

            int cmp = 0;

            for (OrderBy orderBy : orderBys) {
                Comparable compareValue = getCompareValue(orderBy);
                Comparable otherCompareValue = other.getCompareValue(orderBy);

                cmp = CoreObjects.compare(compareValue, otherCompareValue, -1);

                if (orderBy.isReverse()) {
                    cmp = cmp * -1;
                }

                if (cmp != 0) {
                    break;
                }
            }

            return cmp;
        }

        public Comparable getCompareValue(OrderBy orderBy) {

            if (orderByCache.containsKey(orderBy)) {
                return orderByCache.get(orderBy);
            }

            Comparable comparable = orderBy.getComparable(originalValue);
            orderByCache.put(orderBy, comparable);
            return comparable;
        }
    }

    private static abstract class OrderBy {

        private final boolean reverse;

        public OrderBy(boolean reverse) {
            this.reverse = reverse;
        }

        public boolean isReverse() {
            return reverse;
        }

        public abstract Comparable getComparable(Object value);
    }


    private static class PropertyOrderBy extends OrderBy {

        private final CoreProperty property;

        public PropertyOrderBy(CoreProperty property) {
            super(!property.isAscending());
            this.property = property;
        }

        @Override
        public Comparable getComparable(Object value) {
            Object compareValue = property.get(value);

            if (compareValue instanceof Comparable) {
                return (Comparable) compareValue;
            }

            return o -> CoreObjects.compare(value, o, -1);
        }
    }
}

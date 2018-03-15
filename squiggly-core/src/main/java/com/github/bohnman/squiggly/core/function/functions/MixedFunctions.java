package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.core.function.value.BaseCollectionValueHandler;
import com.github.bohnman.squiggly.core.function.value.ValueHandler;

import javax.annotation.Nullable;
import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

@SuppressWarnings("unchecked")
public class MixedFunctions {

    public MixedFunctions() {
    }

    public static Object concat(Object o1, Object o2) {
        if (o1 instanceof String || o2 instanceof String) {
            return CoreStrings.defaultIfEmpty(Objects.toString(o1), "") + CoreStrings.defaultIfEmpty(Objects.toString(o2), "");
        }

        if (o1 == null || o2 == null) {
            return null;
        }

        CoreIndexedIterableWrapper<Object, ?> w1 = CoreIterables.wrap(o1);
        CoreIndexedIterableWrapper<Object, ?> w2 = CoreIterables.wrap(o2);
        return w1.collect(Stream.concat(w1.stream(), w2.stream()));
    }

    public static boolean contains(Object haystack, Object needle) {
        return new ValueHandler<Boolean>() {

            @Override
            protected Boolean handleString(String string) {
                return string.contains(CoreConversions.toString(needle));
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

    public static Object get(Object value, Object key) {
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
                        .filter(pd -> pd.getName().equals(CoreConversions.toString(key)))
                        .map(pd -> CoreMethods.invoke(pd.getReadMethod(), value))
                        .findFirst()
                        .orElse(null);
            }
        }.handle(value);

    }

    public static boolean has(Object value, Object key) {
        if (key == null) {
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
                        .anyMatch(pd -> pd.getName().equals(CoreConversions.toString(key)));
            }
        }.handle(value);
    }

    public static int indexOf(Object haystack, Object needle) {
        return new ValueHandler<Integer>(needle) {
            @Override
            protected Integer handleString(String string) {
                return string.indexOf("" + CoreConversions.toString(needle));
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

    public static Object keys(Object value) {
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
                        .collect(toList());
            }
        }.handle(value);
    }

    public static int lastIndexOf(Object haystack, Object needle) {
        return new ValueHandler<Integer>(needle) {
            @Override
            protected Integer handleString(String string) {
                return string.lastIndexOf("" + CoreConversions.toString(needle));
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

    public static boolean match(Object value, Pattern pattern) {
        return new ValueHandler<Boolean>(pattern) {
            @Override
            protected Boolean handleNull() {
                return false;
            }

            @Override
            protected Boolean handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
                return wrapper.stream().anyMatch(e -> match(e, pattern));
            }

            @Override
            protected Boolean handleString(String string) {
                return pattern.matcher(string).find();
            }

            @Override
            protected Boolean handleObject(Object value) {
                return handleList(CoreBeans.getReadablePropertyDescriptors(value.getClass())
                        .map(PropertyDescriptor::getName)
                        .collect(toList()));
            }
        }.handle(value);
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

    @SquigglyFunctionMethod(aliases = "nmatch")
    public static boolean notMatch(Object o, Pattern pattern) {
        return !match(o, pattern);
    }

    public static Object pick(Object value, Object... keys) {
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
                Map<String, Object> map = toMap(value);
                List<Object> keyList = Arrays.asList(keys);

                return map.entrySet()
                        .stream()
                        .filter(entry -> keyList.contains(entry.getKey()))
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            }
        }.handle(value);

    }

    public static Object pickExcept(Object value, Object... keys) {
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
                Map<String, Object> map = toMap(value);
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

    @SquigglyFunctionMethod(aliases = {"length", "count", "countBy"})
    public static int size(Object value) {
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
                return (int) CoreBeans.getReadablePropertyDescriptors(value.getClass()).count();
            }
        }.handle(value);
    }

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

    @SquigglyFunctionMethod(aliases = "orderBy")
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

    @SuppressWarnings("unchecked")
    static Map<String, Object> toMap(Object value) {
        if (value == null || value instanceof String) {
            return Collections.emptyMap();
        }

        if (value instanceof Map) {
            return (Map) value;
        }

        try {
            return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                    .collect(Collectors.toMap(PropertyDescriptor::getName,
                            pd -> CoreMethods.invoke(pd.getReadMethod(), value)));
        } catch (Exception e) {
            return Collections.emptyMap();
        }
    }

    public static Object values(Object value) {
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
                return CoreLists.of(toMap(value).values());
            }
        }.handle(value);
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

        @SuppressWarnings("unchecked")
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

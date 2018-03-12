package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.function.ValueHandler;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import javax.annotation.Nullable;
import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

@SuppressWarnings("unchecked")
public class MixedFunctions {

    public MixedFunctions() {
    }

    public static Object get(Object value, Object key) {
        if (key == null) {
            return null;
        }

        return new ValueHandler<Object>(key) {

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                Integer actualIndex = normalizeIndex(wrapper.size(), key);
                if (actualIndex == null) return null;
                return wrapper.get(actualIndex);
            }


            @Override
            protected Object handleList(List<Object> list) {
                Integer actualIndex = normalizeIndex(list.size(), key);
                if (actualIndex == null) return null;
                return list.get(actualIndex);
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
            protected Boolean handleArrayWrapper(CoreArrayWrapper wrapper) {
                int size = wrapper.size();
                Integer actualIndex = normalizeIndex(size, key);
                if (actualIndex == null) return false;
                return actualIndex >= 0 && actualIndex < size;
            }


            @Override
            protected Boolean handleList(List<Object> list) {
                int size = list.size();
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

    public static Object keys(Object value) {
        return new ValueHandler<Object>() {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                int size = wrapper.size();
                int[] indexes = new int[size];
                for (int i = 0; i < size; i++) {
                    indexes[i] = i;
                }
                return indexes;
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                int size = CoreIterables.size(iterable);
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

    public static Object limit(Object value, int limit) {
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
            protected Boolean handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper.stream().anyMatch(e -> match(e, pattern));
            }

            @Override
            protected Boolean handleIterable(Iterable<Object> iterable) {
                return CoreStreams.of(iterable).anyMatch(e -> match(e, pattern));
            }

            @Override
            protected Boolean handleString(String string) {
                return pattern.matcher(string).find();
            }

            @Override
            protected Boolean handleObject(Object value) {
                return handleString(CoreConversions.toString(value));
            }
        }.handle(value);
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
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), keys);
                if (actualIndexes.isEmpty()) return wrapper.create(0);
                CoreArrayWrapper newWrapper = wrapper.create(actualIndexes.size());
                for (int i = 0; i < actualIndexes.size(); i++) {
                    newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
                }
                return newWrapper.getArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                List<Integer> actualIndexes = normalizeIndexes(list.size(), keys);
                if (actualIndexes.isEmpty()) return Collections.emptyList();
                List newList = new ArrayList(actualIndexes.size());

                for (Integer actualIndex : actualIndexes) {
                    newList.add(list.get(actualIndex));
                }

                return newList;
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
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), keys);
                if (actualIndexes.isEmpty()) return wrapper.create(0);
                CoreArrayWrapper newWrapper = wrapper.create(Math.max(0, wrapper.size() - actualIndexes.size()));
                int newIdx = 0;

                for (int i = 0; i < wrapper.size(); i++) {
                    if (!actualIndexes.contains(i)) {
                        newWrapper.set(newIdx++, wrapper.get(i));
                    }
                }

                for (int i = 0; i < actualIndexes.size(); i++) {
                    newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
                }
                return newWrapper.getArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                List<Integer> actualIndexes = normalizeIndexes(list.size(), keys);
                if (actualIndexes.isEmpty()) return Collections.emptyList();
                List newList = new ArrayList(Math.max(0, list.size() - actualIndexes.size()));

                for (int i = 0; i < list.size(); i++) {
                    if (!actualIndexes.contains(i)) {
                        newList.add(list.get(i));
                    }
                }

                return newList;
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
                return CoreArrays.wrap(value).reverse().getArray();
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

    @SquigglyFunctionMethod(aliases = "length")
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

    public static Object slice(Object value, int start) {
        return new ValueHandler<Object>(start) {

            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                int len = wrapper.size();
                int realStart = CoreArrays.normalizeIndex(start, len);
                int realEnd = len;
                return (realStart >= realEnd) ? wrapper.create(0).getArray() : wrapper.slice(realStart).getArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                int realStart = CoreArrays.normalizeIndex(start, list.size());
                int realEnd = list.size();
                return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
            }

            @Override
            protected Object handleObject(Object value) {
                return Collections.singletonList(value);
            }

            @Override
            protected Object handleString(String string) {
                return CoreStrings.substring(string, start);
            }
        }.handle(value);
    }

    public static Object slice(Object value, int start, int end) {
        return new ValueHandler<Object>(start, end) {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

            @Override
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                int len = wrapper.size();
                int realStart = CoreArrays.normalizeIndex(start, len);
                int realEnd = CoreArrays.normalizeIndex(end, len);
                return (realStart >= realEnd) ? wrapper.create(0).getArray() : wrapper.slice(realStart, realEnd).getArray();
            }

            @Override
            protected Object handleList(List<Object> list) {
                int realStart = CoreArrays.normalizeIndex(start, list.size());
                int realEnd = CoreArrays.normalizeIndex(end, list.size());
                return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
            }

            @Override
            protected Object handleObject(Object value) {
                return Collections.singletonList(value);
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
            protected Object handleArrayWrapper(CoreArrayWrapper wrapper) {
                List list = wrapper.stream()
                        .map(item -> newComparable(item, orderBys))
                        .sorted()
                        .map(OrderByComparable::getOriginalValue)
                        .collect(toList());

                CoreArrayWrapper newWrapper = wrapper.create(list.size());

                for (int i = 0; i < list.size(); i++) {
                    newWrapper.set(i, list.get(i));
                }

                return newWrapper.getArray();
            }

            @Override
            protected Object handleIterable(Iterable<Object> iterable) {
                return CoreStreams.of(iterable)
                        .map(item -> newComparable(item, orderBys))
                        .sorted()
                        .map(OrderByComparable::getOriginalValue)
                        .collect(toList());
            }

            @Override
            protected Object handleObject(Object value) {
                return value;
            }
        }.handle(value);
    }

    public static Object values(Object value) {
        return new ValueHandler<Object>() {
            @Override
            protected Object handleNull() {
                return Collections.emptyList();
            }

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

                if (compareValue == otherCompareValue) {
                    cmp = 0;
                } else if (compareValue == null) {
                    cmp = -1;
                } else if (otherCompareValue == null) {
                    cmp = 1;
                } else if (compareValue.getClass().isAssignableFrom(otherCompareValue.getClass())) {
                    cmp = compareValue.compareTo(otherCompareValue);
                } else if (otherCompareValue.getClass().isAssignableFrom(compareValue.getClass())) {
                    cmp = otherCompareValue.compareTo(compareValue);
                } else {
                    cmp = compareValue.toString().compareTo(otherCompareValue.toString());
                }

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

            return o -> CoreObjects.firstNonNull(CoreObjects.compare(value, o), -1);
        }
    }
}

package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import javax.annotation.Nullable;
import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public class MixedFunctions {

    public MixedFunctions() {
    }

    @SquigglyMethod
    public static String format(String value, Object... args) {
        if (value == null) {
            return null;
        }

        try {
            return String.format(value, args);
        } catch (IllegalFormatException e) {
            return value;
        }
    }

    public static Object keys(Object value) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int size = wrapper.size();
            int[] indexes = new int[size];
            for (int i = 0; i < size; i++) {
                indexes[i] = i;
            }
            return indexes;
        }

        if (value instanceof Iterable) {
            int size = CoreIterables.size((Iterable) value);
            return IntStream.range(0, size)
                    .boxed()
                    .collect(toList());
        }

        if (value instanceof Map) {
            return ((Map) value).keySet();
        }

        return CoreBeans.getReadablePropertyDescriptors(value.getClass())
                .map(PropertyDescriptor::getName)
                .collect(toList());
    }

    @SquigglyMethod
    public static Object limit(Object value, int limit) {
        if (limit < 0) {
            return slice(value, limit);
        } else {
            return slice(value, 0, limit);
        }
    }

    private static OrderByComparable newComparable(Object value, List<OrderBy> orderBys) {
        return new OrderByComparable(value, orderBys);
    }

    private static List<Integer> normalizeIndexes(int len, Object... indexes) {
        return Stream.of(indexes)
                .flatMap(index -> {
                    if (index instanceof Number) {
                        int actualIndex = CoreArrays.normalizeIndex(((Number) index).intValue(), len, -1, len);
                        return actualIndex < 0 ? Stream.empty() : Stream.of(actualIndex);
                    }

                    if (index instanceof CoreIntRange) {
                        CoreIntRange range = (CoreIntRange) index;
                        int start = CoreArrays.normalizeIndex(CoreObjects.firstNonNull(range.getStart(), 0), len);
                        int end = CoreArrays.normalizeIndex(CoreObjects.firstNonNull(range.getEnd(), len), len);
                        return (start >= end) ? Stream.empty() : IntStream.range(start, end).boxed();
                    }

                    return Stream.empty();
                })
                .distinct()
                .collect(toList());
    }

    @SuppressWarnings("unchecked")
    @SquigglyMethod
    public static Object pick(Object value, Object... indexes) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof String) {
            String string = (String) value;
            List<Integer> actualIndexes = normalizeIndexes(string.length(), indexes);
            if (actualIndexes.isEmpty()) return "";
            StringBuilder builder = new StringBuilder(actualIndexes.size());
            for (Integer actualIndex : actualIndexes) {
                builder.append(string.charAt(actualIndex));
            }

            return builder.toString();
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), indexes);
            if (actualIndexes.isEmpty()) return wrapper.create(0);
            CoreArrayWrapper newWrapper = wrapper.create(actualIndexes.size());
            for (int i = 0; i < actualIndexes.size(); i++) {
                newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
            }
            return newWrapper.getArray();
        }

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : Collections.singletonList(value);
            List<Integer> actualIndexes = normalizeIndexes(list.size(), indexes);
            if (actualIndexes.isEmpty()) return Collections.emptyList();
            List newList = new ArrayList(actualIndexes.size());

            for (Integer actualIndex : actualIndexes) {
                newList.add(list.get(actualIndex));
            }

            return newList;
        }

        return Collections.emptyList();
    }

    @SuppressWarnings("unchecked")
    @SquigglyMethod
    public static Object pickExcept(Object value, Object... indexes) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof String) {
            String string = (String) value;
            List<Integer> actualIndexes = normalizeIndexes(string.length(), indexes);
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

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), indexes);
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

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : Collections.singletonList(value);
            List<Integer> actualIndexes = normalizeIndexes(list.size(), indexes);
            if (actualIndexes.isEmpty()) return Collections.emptyList();
            List newList = new ArrayList(Math.max(0, list.size() - actualIndexes.size()));

            for (int i = 0; i < list.size(); i++) {
                if (!actualIndexes.contains(i)) {
                    newList.add(list.get(i));
                }
            }

            return newList;
        }

        return Collections.emptyList();
    }

    @SquigglyMethod
    public static Object reverse(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            return CoreStrings.reverse((String) value);
        }

        if (value.getClass().isArray()) {
            return CoreArrays.wrap(value).reverse().getArray();
        }

        if (value instanceof Iterable) {
            List list = CoreLists.of((Iterable) value);
            Collections.reverse(list);
            return list;
        }

        return value;
    }

    @SquigglyMethod
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

    @SquigglyMethod
    public static Object slice(Object value, int start) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof String) {
            return CoreStrings.substring((String) value, start);
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int len = wrapper.size();
            int realStart = CoreArrays.normalizeIndex(start, len);
            int realEnd = len;
            return (realStart >= realEnd) ? wrapper.create(0) : wrapper.slice(realStart).getArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        Iterable iterable = (Iterable) value;
        List list = (iterable instanceof List) ? (List) iterable : CoreLists.of(iterable);
        int realStart = CoreArrays.normalizeIndex(start, list.size());
        int realEnd = list.size();
        return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
    }

    @SquigglyMethod
    public static Object slice(Object value, int start, int end) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof String) {
            return CoreStrings.substring((String) value, start, end);
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int len = wrapper.size();
            int realStart = CoreArrays.normalizeIndex(start, len);
            int realEnd = CoreArrays.normalizeIndex(end, len);
            return (realStart >= realEnd) ? wrapper.create(0) : wrapper.slice(realStart, realEnd).getArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        Iterable iterable = (Iterable) value;
        List list = (iterable instanceof List) ? (List) iterable : CoreLists.of(iterable);
        int realStart = CoreArrays.normalizeIndex(start, list.size());
        int realEnd = CoreArrays.normalizeIndex(end, list.size());
        return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
    }

    @SuppressWarnings("unchecked")
    @SquigglyMethod(aliases = "orderBy")
    public static Object sort(Object value, CoreProperty... properties) {
        if (value == null) {
            return null;
        }

        List<OrderBy> orderBys = Arrays.stream(properties)
                .map(PropertyOrderBy::new)
                .collect(toList());

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);

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

        if (value instanceof Iterable) {
            return CoreStreams.of((Iterable<?>) value)
                    .map(item -> newComparable(item, orderBys))
                    .sorted()
                    .map(OrderByComparable::getOriginalValue)
                    .collect(toList());
        }


        return value;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> toMap(Object value) {
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

    @SquigglyMethod
    public static Object values(Object value) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            return value;
        }

        if (value instanceof Iterable) {
            return value;
        }

        return CoreLists.of(toMap(value).values());
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

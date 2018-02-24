package com.github.bohnman.squiggly.function;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.function.annotation.SquigglyMethod;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import com.github.bohnman.squiggly.util.array.ArrayWrapper;
import com.github.bohnman.squiggly.util.array.ArrayWrappers;
import com.github.bohnman.squiggly.util.function.Lambda;
import com.github.bohnman.squiggly.util.range.IntRange;
import com.google.common.base.MoreObjects;
import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.InvocationTargetException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public class DefaultFunctions {

    public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    // Collection Functions
    @SquigglyMethod
    public static Object map(Object value, Lambda lambda) {
        if (value == null || lambda == value) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            ArrayWrapper wrapper = ArrayWrappers.create(value);

            return IntStream.range(0, wrapper.size())
                    .mapToObj(i -> lambda.invoke(wrapper.get(i), i))
                    .toArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        List list = (value instanceof List) ? (List) value : Lists.newArrayList((Iterable) value);

        return IntStream.range(0, list.size())
                .mapToObj(i -> lambda.invoke(list.get(i), i))
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
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            int len = wrapper.size();
            return len == 0 ? null : wrapper.get(0);
        }

        if (value instanceof Iterable) {
            Iterable iterable = (Iterable) value;
            return Iterables.getFirst(iterable, null);
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
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            int len = wrapper.size();
            return len == 0 ? null : wrapper.get(len - 1);
        }

        if (value instanceof Iterable) {
            Iterable iterable = (Iterable) value;
            return Iterables.getLast(iterable, null);
        }

        return value;
    }

    @SquigglyMethod
    public static Object keys(Object value) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value.getClass().isArray()) {
            int length = ArrayWrappers.create(value).size();
            int[] keys = new int[length];
            for (int i = 0; i < length; i++) {
                keys[i] = i;
            }

            return keys;
        }

        if (value instanceof Iterable) {
            int length = Iterables.size((Iterable) value);
            List<Integer> keys = new ArrayList<>(length);

            for (int i = 0; i < length; i++) {
                keys.add(i);
            }

            return keys;
        }

        return Lists.newArrayList(toMap(value).keySet());
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

        return Lists.newArrayList(toMap(value).values());
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
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            List<Integer> actualIndexes = normalizeIndexes(wrapper.size(), indexes);
            if (actualIndexes.isEmpty()) return wrapper.create(0);
            ArrayWrapper newWrapper = wrapper.create(actualIndexes.size());
            for (int i = 0; i < actualIndexes.size(); i++) {
                newWrapper.set(i, wrapper.get(actualIndexes.get(i)));
            }
            return newWrapper.getArray();
        }

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : Lists.newArrayList(value);
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


    @SquigglyMethod
    public static Object reverse(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            return StringUtils.reverse((String) value);
        }

        if (value.getClass().isArray()) {
            return ArrayWrappers.create(value).reverse().getArray();
        }

        if (value instanceof Iterable) {
            List list = Lists.newArrayList((Iterable) value);
            Collections.reverse(list);
            return list;
        }

        return value;
    }

    // TODO: sort/orderBy
    // TODO: filter/where
    // TODO: keys Jtwig

    @SquigglyMethod
    public static Object limit(Object value, int limit) {
        if (limit < 0) {
            return slice(value, limit);
        } else {
            return slice(value, 0, limit);
        }
    }

    @SquigglyMethod
    public static Object slice(Object value, IntRange range) {
        if (range == null) {
            return value;
        }

        int start = MoreObjects.firstNonNull(range.getStart(), 0);

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
            return StringUtils.substring((String) value, start);
        }

        if (value.getClass().isArray()) {
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            int len = wrapper.size();
            int realStart = normalizeIndex(start, len);
            int realEnd = len;
            return (realStart >= realEnd) ? wrapper.create(0) : wrapper.slice(realStart).getArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        Iterable iterable = (Iterable) value;
        List list = (iterable instanceof List) ? (List) iterable : Lists.newArrayList(iterable);
        int realStart = normalizeIndex(start, list.size());
        int realEnd = list.size();
        return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
    }


    @SquigglyMethod
    public static Object slice(Object value, int start, int end) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (value instanceof String) {
            return StringUtils.substring((String) value, start, end);
        }

        if (value.getClass().isArray()) {
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            int len = wrapper.size();
            int realStart = normalizeIndex(start, len);
            int realEnd = normalizeIndex(end, len);
            return (realStart >= realEnd) ? wrapper.create(0) : wrapper.slice(realStart, realEnd).getArray();
        }

        if (!(value instanceof Iterable)) {
            value = Collections.singletonList(value);
        }

        Iterable iterable = (Iterable) value;
        List list = (iterable instanceof List) ? (List) iterable : Lists.newArrayList(iterable);
        int realStart = normalizeIndex(start, list.size());
        int realEnd = normalizeIndex(end, list.size());
        return (realStart >= realEnd) ? Collections.emptyList() : list.subList(realStart, realEnd);
    }

    // String Functions

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

    @SquigglyMethod
    public static String join(Object value, String separator) {
        if (value == null) {
            return null;
        }

        if (separator == null) {
            separator = "";
        }

        if (value.getClass().isArray()) {
            ArrayWrapper wrapper = ArrayWrappers.create(value);
            int len = wrapper.size();
            StringBuilder builder = new StringBuilder();

            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    builder.append(separator);
                }

                builder.append(SquigglyUtils.toString(wrapper.get(i)));
            }

            return builder.toString();
        }

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : Lists.newArrayList((Iterable) value);
            StringBuilder builder = new StringBuilder();

            for (int i = 0; i < list.size(); i++) {
                if (i > 0) {
                    builder.append(separator);
                }

                builder.append(SquigglyUtils.toString(list.get(i)));
            }

            return builder.toString();
        }

        return SquigglyUtils.toString(value);
    }

    public static List<String> split(String value, Object separator) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (separator == null) {
            return Collections.singletonList(value);
        }

        if (separator instanceof String) {
            return Arrays.asList(StringUtils.split(value, (String) separator));
        }

        if (separator instanceof Pattern) {
            return Splitter.on((Pattern) separator).splitToList(value);
        }

        return Collections.singletonList(value);
    }


    @SquigglyMethod
    public static Object parseJson(String json) {
        try {
            return OBJECT_MAPPER.readValue(json, Object.class);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @SquigglyMethod
    public static Object replace(Object object, Object replace) {
        return replace;
    }

    @SquigglyMethod
    public static String replace(String value, Object search, String replace) {
        if (value == null) {
            return null;
        }

        if (search == null) {
            return value;
        }

        if (search instanceof String) {
            return StringUtils.replace(value, (String) search, replace);
        }

        if (search instanceof Pattern) {
            return ((Pattern) search).matcher(value).replaceAll(replace);
        }

        return value;
    }

    @SquigglyMethod
    public static String replaceFirst(String value, Object search, String replace) {
        if (value == null) {
            return null;
        }

        if (search == null) {
            return value;
        }

        if (search instanceof String) {
            return StringUtils.replace(value, (String) search, replace, 1);
        }

        if (search instanceof Pattern) {
            return ((Pattern) search).matcher(value).replaceFirst(replace);
        }

        return value;
    }

    // TODO: concat: JTwig


    @SquigglyMethod(aliases = "capitalise")
    public static String capitalize(String value) {
        return StringUtils.capitalize(value);
    }

    @SquigglyMethod
    public static String lower(String value) {
        return StringUtils.upperCase(value);
    }

    @SquigglyMethod
    public static String trim(String value) {
        return StringUtils.trim(value);
    }

    @SquigglyMethod
    public static String upper(String value) {
        return StringUtils.upperCase(value);
    }

    // Number functions

    @SquigglyMethod
    public static Number abs(Number n) {
        if (n == null) {
            return null;
        }

        return Math.abs(n.doubleValue());
    }

    @SquigglyMethod
    public static Number add(Number n1, Number n2) {
        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() + n2.doubleValue();
    }

    @SquigglyMethod
    public static Number subtract(Number n1, Number n2) {
        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() - n2.doubleValue();
    }

    @SquigglyMethod
    public static Number multiply(Number n1, Number n2) {
        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() * n2.doubleValue();
    }

    @SquigglyMethod
    public static Number divide(Number n1, Number n2) {
        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() / n2.doubleValue();
    }

    @SquigglyMethod
    public static Number ceil(Number n) {
        if (n == null) return null;
        return Math.ceil(n.doubleValue());
    }

    @SquigglyMethod
    public static Number floor(Number n) {
        if (n == null) return null;
        return Math.floor(n.doubleValue());
    }

    @SquigglyMethod
    public static Number round(Number n) {
        if (n == null) return null;
        return Math.round(n.doubleValue());
    }

    @SquigglyMethod
    public static Number min(Number n1, Number n2) {
        if (n1 == null && n2 == null) {
            return null;
        }

        if (n1 == null) {
            return n2;
        }

        if (n2 == null) {
            return n1;
        }

        return Math.min(n1.doubleValue(), n2.doubleValue());
    }

    @SquigglyMethod
    public static Number max(Number n1, Number n2) {
        if (n1 == null && n2 == null) {
            return null;
        }

        if (n1 == null) {
            return n2;
        }

        if (n2 == null) {
            return n1;
        }

        return Math.max(n1.doubleValue(), n2.doubleValue());
    }

    @SquigglyMethod
    public static Number parseNumber(String value, String pattern) {
        try {
            DecimalFormat parser = new DecimalFormat(pattern);
            return parser.parse(value);
        } catch (IllegalArgumentException | ParseException e) {
            return null;
        }
    }

    @SquigglyMethod
    public static Number parseNumber(String value, String pattern, String... otherPatterns) {
        Number number = parseNumber(value, pattern);

        if (number == null) {
            for (String otherPattern : otherPatterns) {
                number = parseNumber(value, otherPattern);

                if (number != null) {
                    break;
                }
            }
        }

        return number;
    }

    //

    // TODO: parseNumber

    //-------------------------------------------------------------------------
    // Object Functions
    //-------------------------------------------------------------------------

    @SquigglyMethod
    public static Object identity(Object object) {
        return object;
    }

    @SquigglyMethod
    public static Object defaultEmpty(Object o1, Object o2) {
        return isEmpty(o1) ? o2 : o1;
    }

    @SquigglyMethod
    public static Object defaultEmpty(Object o1, Object o2, Object... oN) {
        Object value = isEmpty(o1) ? o2 : o1;

        if (isEmpty(value)) {
            for (Object o : oN) {
                if (!isEmpty(o)) {
                    value = o;
                    break;
                }
            }
        }

        return value;
    }

    private static boolean isEmpty(Object o) {
        if (o == null) {
            return true;
        }

        if (o instanceof String && ((String) o).isEmpty()) {
            return true;
        }

        if (o.getClass().isArray() && ArrayWrappers.create(o).isEmpty()) {
            return true;
        }

        if (o instanceof Iterable && Iterables.isEmpty((Iterable) o)) {
            return true;
        }

        return false;
    }


    @SquigglyMethod("default")
    public static Object defaultObject(Object o1, Object o2) {
        return (o1 == null) ? o2 : o1;
    }

    @SquigglyMethod("default")
    public static Object defaultObject(Object o1, Object o2, Object... oN) {
        Object value = (o1 == null) ? o2 : o1;

        if (value == null) {
            for (Object o : oN) {
                if (o != null) {
                    value = o;
                    break;
                }
            }
        }

        return value;
    }

    @SquigglyMethod(aliases = "val")
    public static Object value(Object to) {
        return to;
    }

    @SquigglyMethod(aliases = "val")
    public static Object value(Object input, Object to) {
        return to;
    }


    // Date functions
    // TODO: formatDate appkit
    // TODO: formatDuration appkit
    // TODO: add(Number, String unit)
    // TODO: subtract(Number, String unit)
    // TODO: round(Number, String unit)
    // TODO: ceil(Number, String unit)
    // TODO: floor(Number, String unit)
    // TODO: parseDate


    private static List<Integer> normalizeIndexes(int len, Object... indexes) {
        return Stream.of(indexes)
                .flatMap(index -> {
                    if (index instanceof Number) {
                        int actualIndex = normalizeIndex(((Number) index).intValue(), len, -1, len);
                        return actualIndex < 0 ? Stream.empty() : Stream.of(actualIndex);
                    }

                    if (index instanceof IntRange) {
                        IntRange range = (IntRange) index;
                        int start = normalizeIndex(MoreObjects.firstNonNull(range.getStart(), 0), len);
                        int end = normalizeIndex(MoreObjects.firstNonNull(range.getEnd(), len), len);
                        return (start >= end) ? Stream.empty() : IntStream.range(start, end).boxed();
                    }

                    return Stream.empty();
                })
                .collect(toList());
    }

    private static int normalizeIndex(int index, int length) {
        return normalizeIndex(index, length, 0, length);
    }

    private static int normalizeIndex(int index, int length, int min, int max) {
        if (length == 0) {
            return 0;
        }

        if (index < 0) {
            return Math.max(0, length + index);
        }

        return Math.min(index, length);
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
            return Arrays.stream(Introspector.getBeanInfo(value.getClass()).getPropertyDescriptors())
                    .filter(propertyDescriptor -> propertyDescriptor.getReadMethod() != null)
                    .filter(propertyDescriptor -> !propertyDescriptor.getName().equals("class"))
                    .collect(Collectors.toMap(PropertyDescriptor::getName, pd -> {
                        try {
                            return pd.getReadMethod().invoke(value);
                        } catch (IllegalAccessException | InvocationTargetException e) {
                            throw new RuntimeException(e);
                        }
                    }));
        } catch (Exception e) {
            return Collections.emptyMap();
        }
    }
}

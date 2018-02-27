package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.beans.PropertyDescriptor;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.IdentityHashMap;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public class DefaultFunctions {

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
                .collect(toList());
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
                .collect(toList());
    }

    @SquigglyMethod
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
                .collect(toList());
    }

    @SuppressWarnings("unchecked")
    @SquigglyMethod
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

    // String Functions

    @SquigglyMethod
    public static String join(Object value, String separator) {
        if (value == null) {
            return null;
        }

        if (separator == null) {
            separator = "";
        }

        if (value.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            int len = wrapper.size();
            StringBuilder builder = new StringBuilder();

            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    builder.append(separator);
                }

                builder.append(CoreConversions.toString(wrapper.get(i)));
            }

            return builder.toString();
        }

        if (value instanceof Iterable) {
            List list = (value instanceof List) ? (List) value : CoreLists.of((Iterable) value);
            StringBuilder builder = new StringBuilder();

            for (int i = 0; i < list.size(); i++) {
                if (i > 0) {
                    builder.append(separator);
                }

                builder.append(CoreConversions.toString(list.get(i)));
            }

            return builder.toString();
        }

        return CoreConversions.toString(value);
    }

    public static List<String> split(String value, Object separator) {
        if (value == null) {
            return Collections.emptyList();
        }

        if (separator == null) {
            return Collections.singletonList(value);
        }

        if (separator instanceof String) {
            return Arrays.asList(CoreStrings.split(value, (String) separator));
        }

        if (separator instanceof Pattern) {
            return Arrays.asList(((Pattern) separator).split(value));
        }

        return Collections.singletonList(value);
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
            return CoreStrings.replace(value, (String) search, replace);
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
            return CoreStrings.replace(value, (String) search, replace, 1);
        }

        if (search instanceof Pattern) {
            return ((Pattern) search).matcher(value).replaceFirst(replace);
        }

        return value;
    }

    @SquigglyMethod(aliases = "capitalise")
    public static String capitalize(String value) {
        return CoreStrings.capitalize(value);
    }

    @SquigglyMethod
    public static String lower(String value) {
        return CoreStrings.lower(value);
    }

    @SquigglyMethod
    public static String trim(String value) {
        return CoreStrings.trim(value);
    }

    @SquigglyMethod
    public static String upper(String value) {
        return CoreStrings.upper(value);
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

    //-------------------------------------------------------------------------
    // Object Functions
    //-------------------------------------------------------------------------

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

        if (o.getClass().isArray() && CoreArrays.wrap(o).isEmpty()) {
            return true;
        }

        if (o instanceof Iterable && CoreIterables.isEmpty((Iterable) o)) {
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


    // Date functions
    @SquigglyMethod
    public static ZonedDateTime now() {
        return ZonedDateTime.now();
    }

    @SquigglyMethod
    public static LocalDateTime nowLocal() {
        return LocalDateTime.now();
    }


    @SquigglyMethod
    public static String format(LocalDateTime dateTime) {
        return format(dateTime, "yyyy-MM-dd'T'HH:mm:ss.SSS");
    }

    @SquigglyMethod
    public static String format(LocalDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    @SquigglyMethod
    public static String format(OffsetDateTime dateTime) {
        return format(dateTime.toZonedDateTime(), "yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    }

    @SquigglyMethod
    public static String format(ZonedDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    @SquigglyMethod
    public static LocalDateTime add(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static ZonedDateTime add(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static LocalDateTime subtract(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static ZonedDateTime subtract(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static LocalDateTime ceil(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit).plus(1, temporalUnit);
    }

    @SquigglyMethod
    public static ZonedDateTime ceil(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit).plus(1, temporalUnit);
    }

    @SquigglyMethod
    public static LocalDateTime floor(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit);
    }

    @SquigglyMethod
    public static ZonedDateTime floor(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit);
    }

    @SquigglyMethod
    public static LocalDateTime parseLocalDate(String input) {
        return parseLocalDate(input, "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss", "yyyy-MM-dd'T'HH:mm");
    }

    @SquigglyMethod
    public static LocalDateTime parseLocalDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    @SquigglyMethod
    public static ZonedDateTime parseDate(String input) {
        return parseDate(input, "yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ssZ", "yyyy-MM-dd'T'HH:mmZ", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss", "yyyy-MM-dd'T'HH:mm");
    }

    @SquigglyMethod
    public static ZonedDateTime parseDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault());
    }

    private static Date parseDateInternal(String input, String mainPattern, String... otherPatterns) {

        try {
            return new SimpleDateFormat(mainPattern).parse(input);
        } catch (ParseException e) {
            // ignore;
        }

        for (String otherPattern : otherPatterns) {
            try {
                return new SimpleDateFormat(otherPattern).parse(input);
            } catch (ParseException e) {
                // ignore
            }
        }

        throw new IllegalArgumentException("Unable to parse input");

//        boolean containsZone = false;
//        boolean quoted = false;
//        char[] chars = pattern.toCharArray();
//
//        for (int i = 0; i < chars.length; i++) {
//            char ch = chars[i];
//
//            if (ch == '\'') {
//                if (i == chars.length - 1) break;
//
//                if (!quoted) {
//                    quoted = true;
//                    continue;
//                }
//
//                char nextChar = chars[i + 1];
//
//                if (nextChar == '\'') {
//                    i++;
//                    continue;
//                }
//
//                quoted = false;
//                continue;
//            }
//
//            if (quoted) {
//                continue;
//            }
//
//            if (ch == 'O') {
//                containsZone = true;
//                break;
//            }
//
//            if (ch == 'z') {
//                containsZone = true;
//                break;
//            }
//
//            if (ch == 'X') {
//                containsZone = true;
//                break;
//            }
//
//            if (ch == 'x') {
//                containsZone = true;
//                break;
//            }
//
//            if (ch == 'Z') {
//                containsZone = true;
//                break;
//            }
//        }
//
//
//        try {
//            if (containsZone) {
//                return ZonedDateTime.parse(input, DateTimeFormatter.ofPattern(pattern));
//            }
//
//            else {
//               return LocalDateTime.parse(input, DateTimeFormatter.ofPattern(pattern)).atZone(ZoneId.systemDefault());
//            }
//        } catch (DateTimeParseException | IllegalArgumentException e) {
//            return null;
//        }
    }

    private static TemporalUnit toTemporalUnit(String unit) {
        if (unit == null) {
            return ChronoUnit.MILLIS;
        }

        switch (unit) {
            case "nanos":
            case "nanosecond":
            case "nanoseconds":
            case "ns":
                return ChronoUnit.NANOS;
            case "millisecond":
            case "milliseconds":
            case "millis":
            case "ms":
                return ChronoUnit.MILLIS;
            case "s":
            case "sec":
            case "second":
            case "seconds":
                return ChronoUnit.SECONDS;
            case "min":
            case "minute":
            case "minutes":
            case "m":
                return ChronoUnit.MINUTES;
            case "hr":
            case "hrs":
            case "hour":
            case "hours":
            case "h":
                return ChronoUnit.HOURS;
            case "d":
            case "day":
            case "days":
                return ChronoUnit.DAYS;
            case "M":
            case "mon":
            case "month":
            case "months":
                return ChronoUnit.MONTHS;
            case "y":
            case "yr":
            case "yrs":
            case "year":
            case "years":
                return ChronoUnit.YEARS;
            default:
                throw new IllegalArgumentException("unrecognized temporal unit");
        }
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

    private static OrderByComparable newComparable(Object value, List<OrderBy> orderBys) {
        return new OrderByComparable(value, orderBys);
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
        public int compareTo(Object o) {
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

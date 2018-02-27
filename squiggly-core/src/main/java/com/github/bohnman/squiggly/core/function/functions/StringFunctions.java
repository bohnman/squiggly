package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

public class StringFunctions {

    private StringFunctions() {
    }

    @SquigglyMethod(aliases = "capitalise")
    public static String capitalize(String value) {
        return CoreStrings.capitalize(value);
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

    @SquigglyMethod
    public static String lower(String value) {
        return CoreStrings.lower(value);
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

    @SquigglyMethod
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
    public static String toString(Object value) {
        return CoreConversions.toString(value);
    }

    @SquigglyMethod
    public static String trim(String value) {
        return CoreStrings.trim(value);
    }

    @SquigglyMethod
    public static String upper(String value) {
        return CoreStrings.upper(value);
    }
}

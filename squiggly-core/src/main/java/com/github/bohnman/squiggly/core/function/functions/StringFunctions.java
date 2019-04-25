package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.function.annotations.SquigglyFunctionMethod;

import java.util.Arrays;
import java.util.Collections;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.regex.Pattern;

/**
 * String related functions.
 */
@SuppressWarnings("SameParameterValue")
public class StringFunctions {

    private static final int MAX_PAD_MEMORY = 200;
    private static final int MAX_REPEAT_MEMORY = 500;

    private StringFunctions() {
    }

    /**
     * Capitalize the first letter of a string.
     *
     * @param value string
     * @return capitalized string
     */
    @SquigglyFunctionMethod(aliases = "capitalise")
    public static String capitalize(String value) {
        return CoreStrings.capitalize(value);
    }

    /**
     * Determine if the string contains the search string, ignoring case.
     *
     * @param value  string
     * @param search the search string
     * @return true if contains
     */
    public static boolean containsIgnoreCase(String value, String search) {
        if (value == null || search == null) {
            return false;
        }

        return value.toLowerCase().contains(search.toLowerCase());
    }

    /**
     * Determines if the string ends with the search string.
     *
     * @param value  string
     * @param search search string
     * @return true if ends with
     */
    public static boolean endsWith(String value, String search) {
        if (value == null || search == null) {
            return false;
        }

        return value.endsWith(search);
    }

    /**
     * Format a string.
     *
     * @param value string
     * @param args  arguments
     * @return formatted string
     * @see String#format(String, Object...)
     */
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

    /**
     * Join items in value together using a separator.
     *
     * @param value     collection like object
     * @param separator the item separator
     * @return joined string
     */
    @SquigglyFunctionMethod
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

                builder.append(CoreConversions.safeToString(wrapper.get(i)));
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

                builder.append(CoreConversions.safeToString(list.get(i)));
            }

            return builder.toString();
        }

        return CoreConversions.safeToString(value);
    }

    /**
     * Lowercase a string.
     *
     * @param value string
     * @return lowercased string
     */
    public static String lower(String value) {
        return CoreStrings.lower(value);
    }

    /**
     * Left pad a string up to size using a space.
     * <p>
     * Please note that the size may be restricted to prevent memory attacks.
     *
     * @param value string
     * @param size  max padding
     * @return padded string
     */
    public static String lpad(String value, int size) {
        return lpad(value, size, " ");
    }

    /**
     * Left pad a string up to size using the given pad string.
     * <p>
     * Please note that the size may be restricted to prevent memory attacks.
     *
     * @param value string
     * @param size  max padding
     * @param pad   the padding string
     * @return padded string
     */
    public static String lpad(String value, int size, String pad) {
        return CoreStrings.leftPad(value, restrictPadSize(size, pad), pad);
    }

    /**
     * Left trim a string.
     *
     * @param value string
     * @return trimmed string
     */
    public static String ltrim(String value) {
        return CoreStrings.ltrim(value);
    }

    /**
     * Repeat a string a given number of times.
     * <p>
     * Please note that the number of times might be restricted to prevent memory attacks.
     *
     * @param value a string
     * @param times number of times to repeat.
     * @return repeated string
     */
    public static String repeat(String value, Number times) {
        if (value == null) {
            return null;
        }

        if (times == null) {
            return value;
        }

        int timesInt = times.intValue();

        if (timesInt <= 0) {
            return "";
        }

        StringBuilder builder = new StringBuilder(value.length() * timesInt);
        int repeat = restrictRepeatSize(value, timesInt);

        for (int i = 0; i < repeat; i++) {
            builder.append(value);
        }

        return builder.toString();
    }

    /**
     * Replace the search within the input string with another string.
     *
     * @param value   input string
     * @param search  search string
     * @param replace replacement string
     * @return replaced string
     */
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

    /**
     * Replace only the first occurrence of search within the input string with another string.
     *
     * @param value   input string
     * @param search  search string
     * @param replace replacement string
     * @return replaced string
     */
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

    /**
     * Right pad a string up to size using a space.
     * <p>
     * Please note that the size may be restricted to prevent memory attacks.
     *
     * @param value string
     * @param size  max padding
     * @return padded string
     */
    public static String rpad(String value, int size) {
        return rpad(value, size, " ");
    }

    /**
     * Right pad a string up to size using the given pad string.
     * <p>
     * Please note that the size may be restricted to prevent memory attacks.
     *
     * @param value string
     * @param size  max padding
     * @param pad   the padding string
     * @return padded string
     */
    public static String rpad(String value, int size, String pad) {
        return CoreStrings.rightPad(value, restrictPadSize(size, pad), pad);
    }

    /**
     * Right trim a string.
     *
     * @param value string
     * @return trimmed string
     */
    public static String rtrim(String value) {
        return CoreStrings.rtrim(value);
    }

    /**
     * Split a string into a list.
     *
     * @param value     string
     * @param separator value separator
     * @return list
     */
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

    /**
     * Determine if the string starts with the search string.
     *
     * @param value  string
     * @param search search string
     * @return true if starts with, otherwise false
     */
    public static boolean startsWith(String value, String search) {
        if (value == null || search == null) {
            return false;
        }

        return value.startsWith(search);
    }

    /**
     * Trim a string.
     *
     * @param value string
     * @return trimmed string
     */
    public static String trim(String value) {
        return CoreStrings.trim(value);
    }

    /**
     * Truncate a string longer than maxSize
     *
     * @param value   a string
     * @param maxSize max string size
     * @return truncated string
     */
    public static String truncate(String value, Number maxSize) {
        return truncate(value, maxSize, "");
    }

    /**
     * Truncate a string long than maxSize appending the given value if longer than maxSize.
     *
     * @param value   a string
     * @param maxSize max string size
     * @param append  string to append if greater than max size
     * @return truncated string
     */
    public static String truncate(String value, Number maxSize, String append) {
        if (value == null) {
            return null;
        }

        if (maxSize == null) {
            return value;
        }

        int maxSizeInt = maxSize.intValue();

        if (value.length() <= maxSizeInt) {
            return value;
        }

        return value.substring(0, maxSizeInt) + append;
    }

    /**
     * Uppercase a string
     *
     * @param value string
     * @return uppercased string
     */
    @SquigglyFunctionMethod(aliases = {"uppercase"})
    public static String upper(String value) {
        return CoreStrings.upper(value);
    }

    private static int restrictPadSize(int size, String pad) {
        int maxSize = MAX_PAD_MEMORY / (CoreObjects.firstNonNull(pad, "").length() * 2);
        return Math.min(size, maxSize);
    }

    private static int restrictRepeatSize(String repeat, int size) {
        if (size == 1) {
            return size;
        }

        int maxSize = MAX_REPEAT_MEMORY / (CoreObjects.firstNonNull(repeat, "").length() * 2);
        return Math.max(1, Math.min(size, maxSize));
    }
}

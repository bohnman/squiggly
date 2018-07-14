package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.stream.Stream;

/**
 * Functions dealing with numbers.
 */
public class NumberFunctions {

    private NumberFunctions() {
    }

    /**
     * Return the absolute value of a number.
     *
     * @param n number
     * @return absolute value
     */
    public static Number abs(Number n) {
        if (n == null) {
            return null;
        }

        return Math.abs(n.doubleValue());
    }

    /**
     * Add 2 items together.  For string/collections arguments, this concatenation.  For numbers, addition is performed.
     *
     * @param o1 item 1
     * @param o2 item 2
     * @return sum/concatenation
     */
    public static Object add(Object o1, Object o2) {
        if (o1 instanceof String || o2 instanceof String) {
            return CoreStrings.defaultIfEmpty(CoreConversions.safeToString(o1), "") + CoreStrings.defaultIfEmpty(CoreConversions.safeToString(o2), "");
        }

        if (o1 == null || o2 == null) {
            return null;
        }

        if (CoreIterables.isIterableLike(o1) || CoreIterables.isIterableLike(o2)) {
            CoreIndexedIterableWrapper<Object, ?> w1 = CoreIterables.wrap(o1);
            CoreIndexedIterableWrapper<Object, ?> w2 = CoreIterables.wrap(o2);
            return w1.collect(Stream.concat(w1.stream(), w2.stream()));
        }

        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return cast(n1.doubleValue() + n2.doubleValue());
    }

    /**
     * Cast a number to the appopriate primitive type.  If the number is integer or long like, it will be cast to one
     * of those.  Otherwise a double will be used.
     *
     * @param n number
     * @return casted number
     */
    public static Number cast(Number n) {
        if (n == null) {
            return null;
        }

        if (isIntegerType(n)) {
            return n;
        }

        double doubleValue = n.doubleValue();
        long longValue = n.longValue();

        if ((doubleValue - longValue) == 0) {
            return longValue;
        }

        return doubleValue;
    }

    /**
     * Ceiling a number.
     *
     * @param n a number
     * @return ceiling
     */
    public static Number ceil(Number n) {
        if (n == null) return null;
        return cast(Math.ceil(n.doubleValue()));
    }

    /**
     * Perform division on 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return division result
     */
    @SquigglyFunctionMethod(aliases = "div")
    public static Number divide(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return cast(n1.doubleValue() / n2.doubleValue());
    }

    /**
     * Perform a floor on a number.
     *
     * @param n number
     * @return floor
     */
    public static Number floor(Number n) {
        if (n == null) return null;
        return cast(Math.floor(n.doubleValue()));
    }

    /**
     * Return the max of 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return max
     */
    public static Number max(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null && n2 == null) {
            return null;
        }

        if (n1 == null) {
            return n2;
        }

        if (n2 == null) {
            return n1;
        }

        return cast(Math.max(n1.doubleValue(), n2.doubleValue()));
    }

    /**
     * Return the min of 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return min
     */
    public static Number min(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);


        if (n1 == null && n2 == null) {
            return null;
        }

        if (n1 == null) {
            return n2;
        }

        if (n2 == null) {
            return n1;
        }

        return cast(Math.min(n1.doubleValue(), n2.doubleValue()));
    }

    /**
     * Return the modulus of 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return mod
     */
    @SquigglyFunctionMethod(aliases = "mod")
    public static Number modulus(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return NumberFunctions.cast(n1.doubleValue() % n2.doubleValue());
    }

    /**
     * Return multiplication of 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return multiplication result
     */
    @SquigglyFunctionMethod(aliases = "mul")
    public static Number multiply(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return NumberFunctions.cast(n1.doubleValue() * n2.doubleValue());
    }

    /**
     * Return the number to the given power.
     *
     * @param n   number
     * @param pow power
     * @return n^pow
     */
    public static Number pow(Number n, Number pow) {
        if (n == null) {
            return null;
        }

        if (pow == null) {
            return n;
        }

        return Math.pow(n.doubleValue(), pow.doubleValue());
    }

    /**
     * Round a number.
     *
     * @param n number
     * @return rounded number
     */
    public static Number round(Number n) {
        if (n == null) return null;
        return cast(Math.round(n.doubleValue()));
    }

    /**
     * Square root a number.
     *
     * @param n number
     * @return square root
     */
    public static Number sqrt(Number n) {
        if (n == null) return null;
        return (Math.sqrt(n.doubleValue()));
    }

    /**
     * Subtract 2 numbers.
     *
     * @param o1 number 1
     * @param o2 number 2
     * @return subtraction result
     */
    @SquigglyFunctionMethod(aliases = "sub")
    public static Number subtract(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return NumberFunctions.cast(n1.doubleValue() - n2.doubleValue());
    }

    /**
     * Convert number to a floating point.
     *
     * @param number number
     * @return double
     */
    public static Double toFloat(Number number) {
        return (number == null) ? null : number.doubleValue();
    }

    /**
     * Convert string to a floating point.
     *
     * @param value string
     * @return double
     * @see Double#parseDouble(String)
     */
    public static Double toFloat(String value) {
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Convert string to a floating point using the specified pattern.
     *
     * @param value string
     * @return double
     * @see #toNumber(String, String)
     */
    public static Double toFloat(String value, String pattern) {
        Number number = toNumber(value, pattern);

        if (number == null) {
            return null;
        }

        return number.doubleValue();
    }

    /**
     * Convert number to an integer type.
     *
     * @param number number
     * @return long
     */
    public static Long toInt(Number number) {
        return (number == null) ? null : number.longValue();
    }

    /**
     * Convert a string to an integer type.
     *
     * @param value string
     * @return long
     */
    public static Long toInt(String value) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Convert a string to an integer type using the given pattern.
     *
     * @param value string
     * @return long
     * @see #toNumber(String, String)
     */
    public static Long toInt(String value, String pattern) {
        Number number = toNumber(value, pattern);

        if (number == null) {
            return null;
        }

        return number.longValue();
    }

    /**
     * Convert a string to a number using the given pattern.
     *
     * @param value   string
     * @param pattern pattern
     * @return number or null
     * @see DecimalFormat
     */
    public static Number toNumber(String value, String pattern) {
        try {
            DecimalFormat parser = new DecimalFormat(pattern);
            return parser.parse(value);
        } catch (IllegalArgumentException | ParseException e) {
            return null;
        }
    }

    /**
     * Convert a string to a number using the given pattern2.
     *
     * @param value   string
     * @param pattern pattern
     * @param otherPatterns more patterns
     * @return number or null
     * @see DecimalFormat
     */
    public static Number toNumber(String value, String pattern, String... otherPatterns) {
        Number number = toNumber(value, pattern);

        if (number == null) {
            for (String otherPattern : otherPatterns) {
                number = toNumber(value, otherPattern);

                if (number != null) {
                    break;
                }
            }
        }

        return number;
    }

    private static boolean isIntegerType(Number n) {
        return (n instanceof Long || n instanceof Integer || n instanceof Short || n instanceof Byte);
    }
}

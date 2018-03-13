package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.Objects;
import java.util.stream.Stream;

public class NumberFunctions {

    private NumberFunctions() {
    }

    public static Number abs(Number n) {
        if (n == null) {
            return null;
        }

        return Math.abs(n.doubleValue());
    }

    public static Object add(Object o1, Object o2) {
        if (o1 instanceof String  || o2 instanceof String) {
            return CoreStrings.defaultIfEmpty(Objects.toString(o1), "") + CoreStrings.defaultIfEmpty(Objects.toString(o2), "");
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

    public static Number ceil(Number n) {
        if (n == null) return null;
        return cast(Math.ceil(n.doubleValue()));
    }

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

    public static Number floor(Number n) {
        if (n == null) return null;
        return cast(Math.floor(n.doubleValue()));
    }

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

    public static Number pow(Number n, Number pow) {
        if (n == null) {
            return null;
        }

        if (pow == null) {
            return n;
        }

        return Math.pow(n.doubleValue(), pow.doubleValue());
    }

    public static Number round(Number n) {
        if (n == null) return null;
        return cast(Math.round(n.doubleValue()));
    }

    public static Number sqrt(Number n) {
        if (n == null) return null;
        return (Math.sqrt(n.doubleValue()));
    }

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

    public static Double toFloat(Number number) {
        return (number == null) ? null : number.doubleValue();
    }

    public static Double toFloat(String value) {
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public static Double toFloat(String value, String pattern) {
        Number number = toNumber(value, pattern);

        if (number == null) {
            return null;
        }

        return number.doubleValue();
    }

    public static Long toInt(Number number) {
        return (number == null) ? null : number.longValue();
    }

    public static Long toInt(String value) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public static Long toInt(String value, String pattern) {
        Number number = toNumber(value, pattern);

        if (number == null) {
            return null;
        }

        return number.longValue();
    }

    public static Number toNumber(String value, String pattern) {
        try {
            DecimalFormat parser = new DecimalFormat(pattern);
            return parser.parse(value);
        } catch (IllegalArgumentException | ParseException e) {
            return null;
        }
    }

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

    private static boolean isIntegerType(Number n) {
        return (n instanceof Long || n instanceof Integer || n instanceof Short || n instanceof Byte);
    }

}

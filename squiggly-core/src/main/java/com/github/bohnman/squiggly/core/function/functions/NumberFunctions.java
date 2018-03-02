package com.github.bohnman.squiggly.core.function.functions;

import java.text.DecimalFormat;
import java.text.ParseException;

public class NumberFunctions {

    private NumberFunctions() {
    }

    public static Number abs(Number n) {
        if (n == null) {
            return null;
        }

        return Math.abs(n.doubleValue());
    }

    public static Number ceil(Number n) {
        if (n == null) return null;
        return cast(Math.ceil(n.doubleValue()));
    }

    public static Number floor(Number n) {
        if (n == null) return null;
        return cast(Math.floor(n.doubleValue()));
    }

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

        return cast(Math.max(n1.doubleValue(), n2.doubleValue()));
    }

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

        return cast(Math.min(n1.doubleValue(), n2.doubleValue()));
    }


    public static Number round(Number n) {
        if (n == null) return null;
        return cast(Math.round(n.doubleValue()));
    }

    public static Number sqrt(Number n) {
        if (n == null) return null;
        return (Math.sqrt(n.doubleValue()));
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

    static Number cast(Number n) {
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

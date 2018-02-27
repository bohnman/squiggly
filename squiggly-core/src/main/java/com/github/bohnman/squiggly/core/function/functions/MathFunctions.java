package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.text.DecimalFormat;
import java.text.ParseException;

public class MathFunctions {

    private MathFunctions() {
    }

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

    @SquigglyMethod
    public static Number round(Number n) {
        if (n == null) return null;
        return Math.round(n.doubleValue());
    }
}

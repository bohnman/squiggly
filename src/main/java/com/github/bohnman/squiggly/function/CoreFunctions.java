package com.github.bohnman.squiggly.function;

import com.github.bohnman.squiggly.function.annotation.SquigglyMethod;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import com.github.bohnman.squiggly.util.array.ArrayWrapper;
import com.github.bohnman.squiggly.util.array.ArrayWrappers;
import com.google.common.collect.Streams;

import java.util.Objects;
import java.util.regex.Pattern;

public class CoreFunctions {

    @SquigglyMethod
    public static Object identity(Object object) {
        return object;
    }


    @SquigglyMethod
    public static Number add(Object o1, Object o2) {
        Number n1 = SquigglyUtils.toNumber(o1);
        Number n2 = SquigglyUtils.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() + n2.doubleValue();
    }

    @SquigglyMethod(aliases = "sub")
    public static Number subtract(Object o1, Object o2) {
        Number n1 = SquigglyUtils.toNumber(o1);
        Number n2 = SquigglyUtils.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() - n2.doubleValue();
    }

    @SquigglyMethod(aliases = "mod")
    public static Number modulus(Object o1, Object o2) {
        Number n1 = SquigglyUtils.toNumber(o1);
        Number n2 = SquigglyUtils.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() % n2.doubleValue();
    }

    @SquigglyMethod(aliases = "mul")
    public static Number multiply(Object o1, Object o2) {
        Number n1 = SquigglyUtils.toNumber(o1);
        Number n2 = SquigglyUtils.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() * n2.doubleValue();
    }

    @SquigglyMethod(aliases = "div")
    public static Number divide(Object o1, Object o2) {
        Number n1 = SquigglyUtils.toNumber(o1);
        Number n2 = SquigglyUtils.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() / n2.doubleValue();
    }

    @SquigglyMethod
    public static boolean equals(Object o1, Object o2) {
        return Objects.equals(o1, o2);
    }

    @SquigglyMethod(aliases = "nequals")
    public static boolean notEquals(Object o1, Object o2) {
        return !equals(o1, o2);
    }

    @SquigglyMethod(aliases = "lt")
    public static boolean lessThan(Object o1, Object o2) {
        Integer compare = SquigglyUtils.compare(o1, o2);
        return compare != null && compare < 0;
    }

    @SquigglyMethod(aliases = "lte")
    public static boolean lessThanEquals(Object o1, Object o2) {
        Integer compare = SquigglyUtils.compare(o1, o2);
        return compare != null && compare <= 0;
    }

    @SquigglyMethod(aliases = "gt")
    public static boolean greaterThan(Object o1, Object o2) {
        Integer compare = SquigglyUtils.compare(o1, o2);
        return compare != null && compare > 0;
    }

    @SquigglyMethod(aliases = "gte")
    public static boolean greaterThanEquals(Object o1, Object o2) {
        Integer compare = SquigglyUtils.compare(o1, o2);
        return compare != null && compare >= 0;
    }

    @SquigglyMethod
    @SuppressWarnings("unchecked")
    public static boolean match(Object o, Pattern pattern) {

        if (o == null) {
            return false;
        }
        
        if (o.getClass().isArray()) {
            ArrayWrapper wrapper = ArrayWrappers.create(o);
            return wrapper.stream().anyMatch(e -> match(e, pattern));
        }

        if (o instanceof Iterable) {
            return Streams.stream((Iterable) o).anyMatch(e -> match(e, pattern));
        }
        
        if (!(o instanceof String)) {
            o = SquigglyUtils.toString(o);
        }

        return pattern.matcher((String) o).find();
    }

    @SquigglyMethod(aliases = "nmatch")
    public static boolean notMatch(Object o, Pattern pattern) {
        return !match(o, pattern);
    }

    @SquigglyMethod
    public static boolean or(Object o1, Object o2) {
        return SquigglyUtils.toBoolean(o1) || SquigglyUtils.toBoolean(o2);
    }

    @SquigglyMethod
    public static boolean and(Object o1, Object o2) {
        return SquigglyUtils.toBoolean(o1) && SquigglyUtils.toBoolean(o2);
    }

    @SquigglyMethod
    public static boolean not(Object o) {
        return !SquigglyUtils.toBoolean(o);
    }
}
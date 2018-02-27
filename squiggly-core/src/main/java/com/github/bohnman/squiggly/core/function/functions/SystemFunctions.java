package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.util.Objects;
import java.util.regex.Pattern;

public class SystemFunctions {

    private SystemFunctions() {
    }

    @SquigglyMethod
    public static Number add(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() + n2.doubleValue();
    }

    @SquigglyMethod
    public static boolean and(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) && CoreConversions.toBoolean(o2);
    }

    @SquigglyMethod(aliases = "div")
    public static Number divide(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

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

    @SquigglyMethod(aliases = "gt")
    public static boolean greaterThan(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare > 0;
    }

    @SquigglyMethod(aliases = "gte")
    public static boolean greaterThanEquals(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare >= 0;
    }

    @SquigglyMethod
    public static Object identity(Object object) {
        return object;
    }

    @SquigglyMethod(aliases = "lt")
    public static boolean lessThan(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare < 0;
    }

    @SquigglyMethod(aliases = "lte")
    public static boolean lessThanEquals(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare <= 0;
    }

    @SquigglyMethod
    @SuppressWarnings("unchecked")
    public static boolean match(Object o, Pattern pattern) {

        if (o == null) {
            return false;
        }

        if (o.getClass().isArray()) {
            CoreArrayWrapper wrapper = CoreArrays.wrap(o);
            return wrapper.stream().anyMatch(e -> match(e, pattern));
        }

        if (o instanceof Iterable) {
            return CoreStreams.of((Iterable) o).anyMatch(e -> match(e, pattern));
        }

        if (!(o instanceof String)) {
            o = CoreConversions.toString(o);
        }

        return pattern.matcher((String) o).find();
    }

    @SquigglyMethod(aliases = "mod")
    public static Number modulus(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

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
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return 0;
        }

        return n1.doubleValue() * n2.doubleValue();
    }

    @SquigglyMethod
    public static boolean not(Object o) {
        return !CoreConversions.toBoolean(o);
    }

    @SquigglyMethod(aliases = "nequals")
    public static boolean notEquals(Object o1, Object o2) {
        return !equals(o1, o2);
    }

    @SquigglyMethod(aliases = "nmatch")
    public static boolean notMatch(Object o, Pattern pattern) {
        return !match(o, pattern);
    }

    @SquigglyMethod
    public static boolean or(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) || CoreConversions.toBoolean(o2);
    }

    @SquigglyMethod(aliases = "sub")
    public static Number subtract(Object o1, Object o2) {
        Number n1 = CoreConversions.toNumber(o1);
        Number n2 = CoreConversions.toNumber(o2);

        if (n1 == null) {
            return null;
        }

        if (n2 == null) {
            return n1;
        }

        return n1.doubleValue() - n2.doubleValue();
    }
}
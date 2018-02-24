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
        Number n1 = toNumber(o1);
        Number n2 = toNumber(o2);

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
        Number n1 = toNumber(o1);
        Number n2 = toNumber(o2);

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
        Number n1 = toNumber(o1);
        Number n2 = toNumber(o2);

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
        Number n1 = toNumber(o1);
        Number n2 = toNumber(o2);

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
        Number n1 = toNumber(o1);
        Number n2 = toNumber(o2);

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
        Integer compare = compare(o1, o2);
        return compare != null && compare < 0;
    }

    @SquigglyMethod(aliases = "lte")
    public static boolean lessThanEquals(Object o1, Object o2) {
        Integer compare = compare(o1, o2);
        return compare != null && compare <= 0;
    }

    @SquigglyMethod(aliases = "gt")
    public static boolean greaterThan(Object o1, Object o2) {
        Integer compare = compare(o1, o2);
        return compare != null && compare > 0;
    }

    @SquigglyMethod(aliases = "gte")
    public static boolean greaterThanEquals(Object o1, Object o2) {
        Integer compare = compare(o1, o2);
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
        return toBoolean(o1) || toBoolean(o2);
    }

    @SquigglyMethod
    public static boolean and(Object o1, Object o2) {
        return toBoolean(o1) && toBoolean(o2);
    }

    @SquigglyMethod
    public static boolean not(Object o) {
        return !toBoolean(o);
    }
    
    public static Number toNumber(Object o) {
        return toNumber(o, 0);
    }
    
    public static Number toNumber(Object o, Number defaultValue) {
        if (o == null) {
            return defaultValue;
        }
        
        if (o instanceof Number) {
            return (Number) o;
        }
        
        if (o instanceof String) {
            try {
                return Double.parseDouble((String) o);
            } catch (NumberFormatException e) {
                return defaultValue;
            }
        }
        
        return defaultValue;
    }

    @SuppressWarnings("unchecked")
    public static boolean toBoolean(Object o) {
        if (o == null) {
            return false;
        }
        
        if (o instanceof Boolean) {
            return (Boolean) o;
        }

        if (o instanceof Number) {
            return ((Number) o).doubleValue() != 0;
        }

        if (o instanceof String) {
            return !"".equals(o);
        }

        return false;
    }


    @SuppressWarnings("unchecked")
    private static Integer compare(Object o1, Object o2) {
        if (equals(o1, o2)) {
            return 0;
        }

        if (o1 == null || o2 == null) {
            return null;
        }

        if (o1 instanceof Number && o2 instanceof Number) {
            double d1 = ((Number) o1).doubleValue();
            double d2 = ((Number) o2).doubleValue();
            return Double.compare(d1, d2);
        }

        if (!(o1 instanceof Comparable)) {
            return null;
        }

        if (!(o2 instanceof Comparable)) {
            return null;
        }

        if (!o1.getClass().isAssignableFrom(o2.getClass())) {
            return null;
        }

        Comparable c1 = (Comparable) o1;
        Comparable c2 = (Comparable) o2;

        return c1.compareTo(c2);
    }
}
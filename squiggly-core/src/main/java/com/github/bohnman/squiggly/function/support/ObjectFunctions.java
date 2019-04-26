package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.library.CoreLibraries;
import com.github.bohnman.squiggly.function.SquigglyFunctionMethod;
import com.github.bohnman.squiggly.function.ValueHandler;

import java.time.*;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Generic object functions.
 */
@SuppressWarnings("unchecked")
public class ObjectFunctions {

    /**
     * Performs a logic and between to functions.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return logical and
     */
    public static boolean and(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) && CoreConversions.toBoolean(o2);
    }

    /**
     * Determines if value is between a range.
     *
     * @param value          input
     * @param startInclusive start inclusive
     * @param endExclusive   end inclusive
     * @return between
     */
    public static boolean between(Object value, Object startInclusive, Object endExclusive) {
        Integer c1 = CoreObjects.compare(startInclusive, value);

        if (c1 == null || c1 > 0) {
            return false;
        }

        Integer c2 = CoreObjects.compare(endExclusive, value);

        if (c2 == null || c2 <= 0) {
            return false;
        }

        return true;
    }

    /**
     * Just return newValue.
     *
     * @param object   any object
     * @param newValue value to return
     * @return newValue
     */
    @SquigglyFunctionMethod
    public static Object assign(Object object, Object newValue) {
        return newValue;
    }

    /**
     * Is the value an iterable or array type.
     *
     * @param value object
     * @return true if iterable or array
     */
    public static boolean isIterableOrArray(Object value) {
        return isIterable(value) || isArray(value);
    }

    /**
     * Pick the first non-empty object.
     *
     * @param o1 object 1
     * @param o2 object 2.
     * @return first non-empty object
     */
    public static Object defaultEmpty(Object o1, Object o2) {
        return isEmpty(o1) ? o2 : o1;
    }

    /**
     * Pick the first non-empty object.
     *
     * @param o1 object 1
     * @param o2 object 2.
     * @param oN other objects.
     * @return first non-empty object
     */
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

    /**
     * Pick the first non-null object.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return first non-null
     */
    @SquigglyFunctionMethod("default")
    public static Object defaultObject(Object o1, Object o2) {
        return (o1 == null) ? o2 : o1;
    }

    /**
     * Pick the first non-null object.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @param oN other objects
     * @return first non-null
     */
    @SquigglyFunctionMethod("default")
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

    /**
     * Determine if 2 objects are equal.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true if equals
     */
    public static boolean equals(Object o1, Object o2) {
        return CoreObjects.equals(o1, o2);
    }

    /**
     * Determine if object 1 is greater than object 2.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true of greater than
     */
    @SquigglyFunctionMethod(aliases = "gt")
    public static boolean greaterThan(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare > 0;
    }

    /**
     * Determine if object 1 is greater than or equal to object 2.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true of greater than or equal to
     */
    @SquigglyFunctionMethod(aliases = "gte")
    public static boolean greaterThanEquals(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare >= 0;
    }

    /**
     * Get the identity lambda.
     *
     * @return identity
     */
    public static CoreLambda identity() {
        return CoreLambda.identity();
    }

    /**
     * If value is true, return the trueValue else the falseValue.
     *
     * @param value      test
     * @param trueValue  value when test is true
     * @param falseValue value when test is false
     * @return trueValue or falseValue
     */
    public static Object ifelse(Boolean value, Object trueValue, Object falseValue) {
        return value ? trueValue : falseValue;
    }

    /**
     * If value is true, return the trueValue else the falseValue.
     *
     * @param value      test
     * @param trueValue  value when test is true
     * @param falseValue value when test is false
     * @return trueValue or falseValue
     */
    public static Object ifelse(Object value, Predicate predicate, Object trueValue, Object falseValue) {
        return predicate.test(value) ? trueValue : falseValue;
    }

    /**
     * Determine if value is an array.
     *
     * @param value object
     * @return true if array
     */
    public static boolean isArray(Object value) {
        return (value != null && value.getClass().isArray());
    }

    /**
     * Determine if value is a boolean.
     *
     * @param value object
     * @return true if boolean
     */
    public static boolean isBoolean(Object value) {
        return value instanceof Boolean;
    }

    /**
     * Determine if value is a date type.
     *
     * @param value object
     * @return true if date type
     */
    public static boolean isDate(Object value) {
        if (value instanceof Date) {
            return true;
        }

        if (value instanceof ZonedDateTime) {
            return true;
        }

        if (value instanceof OffsetDateTime) {
            return true;
        }

        if (value instanceof LocalDateTime) {
            return true;
        }

        if (value instanceof LocalDate) {
            return true;
        }

        if (value instanceof LocalTime) {
            return true;
        }

        if (value instanceof Instant) {
            return true;
        }

        if (value instanceof Calendar) {
            return true;
        }

        if (CoreLibraries.isJodaTimePresent()) {
            return true;
        }

        return false;
    }

    /**
     * Determine if value is an iterable.
     *
     * @param value object
     * @return true if iterable
     */
    public static boolean isIterable(Object value) {
        return value instanceof Iterable;
    }

    /**
     * Determine if value is a map.
     *
     * @param value object
     * @return true if map
     */
    public static boolean isMap(Object value) {
        return value instanceof Map;
    }

    /**
     * Determine if value is null.
     *
     * @param value object
     * @return true if null
     */
    public static boolean isNull(Object value) {
        return value == null;
    }

    /**
     * Determine if value is a number.
     *
     * @param value object
     * @return true if number
     */
    public static boolean isNumber(Object value) {
        return value instanceof Number;
    }

    /**
     * Determine if value is a plain java object.
     *
     * @param value object
     * @return true if object
     */
    public static boolean isObject(Object value) {
        if (value == null) {
            return false;
        }

        if (isIterableOrArray(value)) {
            return false;
        }

        if (isBoolean(value)) {
            return false;
        }

        if (isNumber(value)) {
            return false;
        }

        if (isMap(value)) {
            return false;
        }

        if (isString(value)) {
            return false;
        }

        if (isDate(value)) {
            return false;
        }

        if (value instanceof Character) {
            return false;
        }

        return true;
    }

    /**
     * Determine if value is a set.
     *
     * @param value object
     * @return true if set
     */
    public static boolean isSet(Object value) {
        return value instanceof Set;
    }

    /**
     * Determine if value is a string.
     *
     * @param value object
     * @return true if set
     */
    public static boolean isString(Object value) {
        return value instanceof String;
    }

    /**
     * Determine if object 1 is less than object 2
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true if less than
     */
    @SquigglyFunctionMethod(aliases = "lt")
    public static boolean lessThan(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare < 0;
    }

    /**
     * Determine if object 1 is less than or equals to object 2
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true if less than or equal to
     */
    @SquigglyFunctionMethod(aliases = "lte")
    public static boolean lessThanEquals(Object o1, Object o2) {
        Integer compare = CoreObjects.compare(o1, o2);
        return compare != null && compare <= 0;
    }

    /**
     * Perform a logical not to an object.
     *
     * @param o object
     * @return logical not
     */
    public static boolean not(Object o) {
        return !CoreConversions.toBoolean(o);
    }

    /**
     * Determine if object 1 does not equal to object 2.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return true if not equals
     */
    @SquigglyFunctionMethod(aliases = "nequals")
    public static boolean notEquals(Object o1, Object o2) {
        return !equals(o1, o2);
    }

    /**
     * Performs a logical or between 2 objects.
     *
     * @param o1 object 1
     * @param o2 object 2
     * @return logical or
     */
    public static boolean or(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) || CoreConversions.toBoolean(o2);
    }

    /**
     * Just return what is passed in.
     *
     * @param object any object
     * @return the object
     */
    public static Object self(Object object) {
        return object;
    }

    private static boolean isEmpty(Object o) {
        return new ValueHandler<Boolean>() {
            @Override
            protected Boolean handleNull() {
                return true;
            }

            @Override
            protected Boolean handleArrayWrapper(CoreArrayWrapper wrapper) {
                return wrapper.isEmpty();
            }

            @Override
            protected Boolean handleIterable(Iterable<Object> iterable) {
                return CoreIterables.isEmpty(iterable);
            }

            @Override
            protected Boolean handleObject(Object value) {
                return false;
            }
        }.handle(o);
    }

    private static class InternalJodaFunctions {

        public boolean isDate(Object value) {
            if (value instanceof org.joda.time.DateTime) {
                return true;
            }

            if (value instanceof org.joda.time.Instant) {
                return true;
            }

            if (value instanceof org.joda.time.LocalDateTime) {
                return true;
            }

            if (value instanceof org.joda.time.LocalDate) {
                return true;
            }

            if (value instanceof org.joda.time.LocalTime) {
                return true;
            }

            return false;
        }
    }

}

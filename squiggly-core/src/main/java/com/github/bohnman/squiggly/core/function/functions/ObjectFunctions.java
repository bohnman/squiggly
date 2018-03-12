package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.library.CoreLibraries;
import com.github.bohnman.squiggly.core.function.ValueHandler;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;

@SuppressWarnings("unchecked")
public class ObjectFunctions {

    public static boolean and(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) && CoreConversions.toBoolean(o2);
    }

    @SquigglyMethod
    public static Object assign(Object object, Object newValue) {
        return newValue;
    }

    public static Object defaultEmpty(Object o1, Object o2) {
        return isEmpty(o1) ? o2 : o1;
    }

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

    @SquigglyMethod("default")
    public static Object defaultObject(Object o1, Object o2) {
        return (o1 == null) ? o2 : o1;
    }

    @SquigglyMethod("default")
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

    public static Object identity(Object object) {
        return object;
    }

    public static Object ifelse(Boolean value, Object trueValue, Object falseValue) {
        return value ? trueValue : falseValue;
    }

    public static Object ifelse(Object value, Predicate predicate, Object trueValue, Object falseValue) {
        return predicate.test(value) ? trueValue : falseValue;
    }

    public static boolean canBeIterated(Object value) {
        return isIterable(value) || isArray(value);
    }

    public static boolean isArray(Object value) {
        return (value != null && value.getClass().isArray());
    }

    public static boolean isBoolean(Object value) {
        return value instanceof Boolean;
    }

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

    public static boolean isIterable(Object value) {
        return value instanceof Iterable;
    }

    public static boolean isMap(Object value) {
        return value instanceof Map;
    }

    public static boolean isNull(Object value) {
        return value == null;
    }

    public static boolean isNumber(Object value) {
        return value instanceof Number;
    }

    public static boolean isObject(Object value) {
        if (value == null) {
            return false;
        }

        if (canBeIterated(value)) {
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

    public static boolean isSet(Object value) {
        return value instanceof Set;
    }

    public static boolean isString(Object value) {
        return value instanceof String;
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


    public static boolean not(Object o) {
        return !CoreConversions.toBoolean(o);
    }

    @SquigglyMethod(aliases = "nequals")
    public static boolean notEquals(Object o1, Object o2) {
        return !equals(o1, o2);
    }

    public static boolean or(Object o1, Object o2) {
        return CoreConversions.toBoolean(o1) || CoreConversions.toBoolean(o2);
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

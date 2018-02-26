package com.github.bohnman.core.convert;

import com.github.bohnman.core.array.CoreArrayWrappers;

import javax.annotation.Nullable;
import java.util.function.Function;
import java.util.function.Predicate;

public class CoreConversions {

    @Nullable
    public static String toString(@Nullable Object o) {
        if (o == null) {
            return null;
        }

        if (o.getClass().isArray()) {
            return CoreArrayWrappers.create(o).toString();
        }

        return o.toString();
    }

    @Nullable
    public static Number toNumber(@Nullable Object o) {
        return toNumber(o, 0);
    }

    @Nullable
    public static Number toNumber(@Nullable Object o, @Nullable Number defaultValue) {
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
    @Nullable
    public static boolean toBoolean(@Nullable Object o) {
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
    public static Function toFunction(Object o) {
        if (o instanceof Function) {
            return (Function) o;
        }

        if (o instanceof Predicate) {
            return ((Predicate) o)::test;
        }

        return (in) -> o;
    }

    @SuppressWarnings("unchecked")
    public static Predicate toPredicate(Object o) {
        if (o instanceof Predicate) {
            return (Predicate) o;
        }

        if (o instanceof Function) {
            return (in) -> toBoolean(((Function) o).apply(in));
        }

        return (in) -> toBoolean(o);
    }
}

package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

public class ObjectFunctions {

    @SquigglyMethod
    public static Object replace(Object object, Object replace) {
        return replace;
    }

    @SquigglyMethod
    public static Object defaultEmpty(Object o1, Object o2) {
        return isEmpty(o1) ? o2 : o1;
    }

    @SquigglyMethod
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

    private static boolean isEmpty(Object o) {
        if (o == null) {
            return true;
        }

        if (o instanceof String && ((String) o).isEmpty()) {
            return true;
        }

        if (o.getClass().isArray() && CoreArrays.wrap(o).isEmpty()) {
            return true;
        }

        if (o instanceof Iterable && CoreIterables.isEmpty((Iterable) o)) {
            return true;
        }

        return false;
    }
}

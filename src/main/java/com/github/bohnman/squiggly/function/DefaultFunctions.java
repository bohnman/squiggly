package com.github.bohnman.squiggly.function;

import com.github.bohnman.squiggly.function.annotation.SquigglyMethod;
import com.google.common.collect.Lists;

import java.util.Arrays;

public class DefaultFunctions {

    @SquigglyMethod
    public static Object limit(Object value, int limit) {
        if (value instanceof Iterable) {
            return Lists.newArrayList((Iterable) value).subList(0, limit);
        }

        return value;
    }

    @SquigglyMethod
    public static Object foo(Object value, String... args) {
        System.out.println("foo" + Arrays.toString(args));
        return value;
    }

    @SquigglyMethod
    public static Object foo(Object value, Integer num) {
        System.out.println("foo2" + num);
        return value;
    }

}

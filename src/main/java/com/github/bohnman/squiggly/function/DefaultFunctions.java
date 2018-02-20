package com.github.bohnman.squiggly.function;

import com.github.bohnman.squiggly.Person;
import com.github.bohnman.squiggly.function.annotation.SquigglyFunction;

import java.util.Collections;
import java.util.List;

public class DefaultFunctions {

    @SquigglyFunction
    public static Object limit(Object value) {
//        if (value instanceof List) {
//            return ((List) value).subList(0, 1);
//        }

        return Collections.singletonList(new Person("Ryan", "Bohn"));
    }
}

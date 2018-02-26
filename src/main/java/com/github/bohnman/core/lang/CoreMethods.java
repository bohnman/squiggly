package com.github.bohnman.core.lang;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class CoreMethods {

    private CoreMethods() {
    }


    public static Object invoke(Method method, Object owner, Object... args) {
        try {
            return method.invoke(owner, args);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

}

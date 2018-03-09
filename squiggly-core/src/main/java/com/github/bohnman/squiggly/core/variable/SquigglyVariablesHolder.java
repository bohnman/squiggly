package com.github.bohnman.squiggly.core.variable;

import java.util.Map;

public class SquigglyVariablesHolder {

    private static final ThreadLocal<Map<String, Object>> THREAD_LOCAL = new ThreadLocal<>();

    public static Map<String, Object> get() {
        return THREAD_LOCAL.get();
    }

    public static void set(Map<String, Object> variables) {
        THREAD_LOCAL.set(variables);
    }

    public static void remove() {
        THREAD_LOCAL.remove();
    }

}
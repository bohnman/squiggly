package com.github.bohnman.squiggly.core.filter;

public class SquigglyFilterHolder {

    private static final ThreadLocal<String> THREAD_LOCAL = new ThreadLocal<>();

    public static String get() {
        return THREAD_LOCAL.get();
    }

    public static void set(String filter) {
        THREAD_LOCAL.set(filter);
    }

    public static void remove() {
        THREAD_LOCAL.remove();
    }

}

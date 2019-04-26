package com.github.bohnman.squiggly.variable.support;

import javax.annotation.Nullable;
import java.util.Map;

/**
 * Thread local variable map holder.
 */
public class SquigglyVariablesHolder {

    private static final ThreadLocal<Map<String, Object>> THREAD_LOCAL = new ThreadLocal<>();

    /**
     * Get the map from the thread local.
     *
     * @return map
     */
    @Nullable
    public static Map<String, Object> get() {
        return THREAD_LOCAL.get();
    }

    /**
     * Set the map in the thread local.
     *
     * @param variables map
     */
    public static void set(Map<String, Object> variables) {
        THREAD_LOCAL.set(variables);
    }

    /**
     * Remove the map from the thread local.
     */
    public static void remove() {
        THREAD_LOCAL.remove();
    }

}
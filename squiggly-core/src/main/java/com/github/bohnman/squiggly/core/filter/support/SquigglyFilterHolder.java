package com.github.bohnman.squiggly.core.filter.support;

/**
 * A thread local object used to hold a Squiggly filter string.
 */
public class SquigglyFilterHolder {

    private static final ThreadLocal<String> THREAD_LOCAL = new ThreadLocal<>();

    /**
     * Get the filter string or null if there is none.
     *
     * @return filter or null
     */
    public static String get() {
        return THREAD_LOCAL.get();
    }

    /**
     * Set the filter string in the thread local.
     *
     * @param filter filter string
     */
    public static void set(String filter) {
        THREAD_LOCAL.set(filter);
    }

    /**
     * Remove the filter string in the thread local.
     */
    public static void remove() {
        THREAD_LOCAL.remove();
    }

}

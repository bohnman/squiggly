package dev.nicklasw.squiggly.context.provider;

import net.jcip.annotations.ThreadSafe;

/**
 * Provides a thread-local for holding a servlet request.
 */
@ThreadSafe
public class SquigglyFilterHolder {
    private static final ThreadLocal<String> HOLDER = new ThreadLocal<>();

    public static String getFilter() {
        return HOLDER.get();
    }

    public static void setFilter(String filter) {
        HOLDER.set(filter);
    }

    public static void removeFilter() {
        HOLDER.remove();
    }
}

package com.github.bohnman.squiggly.extension.servlet.support;

import javax.annotation.concurrent.ThreadSafe;
import javax.servlet.http.HttpServletResponse;

/**
 * Provides a thread-local for holding a servlet response.
 */
@ThreadSafe
public class SquigglyResponseHolder {
    private static final ThreadLocal<HttpServletResponse> HOLDER = new ThreadLocal<>();

    public static HttpServletResponse getResponse() {
        return HOLDER.get();
    }

    public static void setResponse(HttpServletResponse response) {
        HOLDER.set(response);
    }

    public static void removeResponse() {
        HOLDER.remove();
    }
}

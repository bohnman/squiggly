package com.github.bohnman.squiggly.web;

import net.jcip.annotations.ThreadSafe;

import javax.servlet.http.HttpServletRequest;

/**
 * Provides a thread-local for holding a servlet request.
 */
@ThreadSafe
public class SquigglyRequestHolder {
    private static final ThreadLocal<HttpServletRequest> HOLDER = new ThreadLocal<HttpServletRequest>();

    public static HttpServletRequest getRequest() {
        return HOLDER.get();
    }

    public static void setRequest(HttpServletRequest request) {
        HOLDER.set(request);
    }

    public static void removeRequest() {
        HOLDER.remove();
    }
}

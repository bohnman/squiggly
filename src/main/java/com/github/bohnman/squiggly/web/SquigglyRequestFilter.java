package com.github.bohnman.squiggly.web;

import net.jcip.annotations.ThreadSafe;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Servlet filter that sets the request on the {@link SquigglyRequestHolder}.
 */
@ThreadSafe
public class SquigglyRequestFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain) throws IOException, ServletException {

        if (!(request instanceof HttpServletRequest) || !(response instanceof HttpServletResponse)) {
            filterChain.doFilter(request, response);
            return;
        }

        SquigglyRequestHolder.setRequest((HttpServletRequest) request);
        SquigglyResponseHolder.setResponse((HttpServletResponse) response);

        try {
            filterChain.doFilter(request, response);
        } finally {
            SquigglyRequestHolder.removeRequest();
            SquigglyResponseHolder.removeResponse();
        }
    }

    @Override
    public void destroy() {
    }
}

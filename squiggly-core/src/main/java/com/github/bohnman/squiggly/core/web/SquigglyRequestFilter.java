package com.github.bohnman.squiggly.core.web;

import com.github.bohnman.squiggly.core.variable.SquigglyVariablesHolder;

import javax.annotation.concurrent.ThreadSafe;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

/**
 * Servlet filter that sets the request on the {@link SquigglyRequestHolder}.
 */
@ThreadSafe
public class SquigglyRequestFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) {
    }

    @SuppressWarnings("unchecked")
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain) throws IOException, ServletException {

        if (!(request instanceof HttpServletRequest)) {
            filterChain.doFilter(request, response);
            return;
        }

        HttpServletRequest httpRequest = (HttpServletRequest) request;
        SquigglyRequestHolder.setRequest(httpRequest);
        SquigglyVariablesHolder.set(toVariables(httpRequest));

        try {
            filterChain.doFilter(request, response);
        } finally {
            SquigglyRequestHolder.removeRequest();
            SquigglyVariablesHolder.remove();
        }
    }

    private Map<String, Object> toVariables(HttpServletRequest httpRequest) {
        Map<String, Object> variables = new HashMap<>();
        Enumeration parameterNames = httpRequest.getParameterNames();


        while (parameterNames.hasMoreElements()) {
            String name = parameterNames.nextElement().toString();
            Object value = httpRequest.getParameter(name);
            variables.put(name, value);
        }

        return variables;
    }

    @Override
    public void destroy() {
    }
}

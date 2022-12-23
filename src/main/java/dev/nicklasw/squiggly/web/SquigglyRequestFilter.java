package dev.nicklasw.squiggly.web;

import net.jcip.annotations.ThreadSafe;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
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

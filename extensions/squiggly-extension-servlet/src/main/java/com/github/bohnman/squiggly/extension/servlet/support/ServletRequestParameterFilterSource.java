package com.github.bohnman.squiggly.extension.servlet.support;

import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.filter.SquigglyFilters;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;

import static java.util.Objects.requireNonNull;

public class ServletRequestParameterFilterSource extends SquigglyFilters.SingleNamedFilterSource {

    public static final String DEFAULT_PARAMETER_NAME = "fields";

    private final String parameterName;

    private ServletRequestParameterFilterSource(String filterName, String parameterName) {
        super(filterName);
        this.parameterName = requireNonNull(parameterName);
    }

    @Nullable
    @Override
    protected String getFilter() {
        HttpServletRequest request = ServletRequestHolder.getRequest();

        if (request == null) {
            return null;
        }

        return request.getParameter(parameterName);
    }

    public static ServletRequestParameterFilterSource createContexted() {
        return new ServletRequestParameterFilterSource(SquigglyFilterSource.CONTEXT_NAME, DEFAULT_PARAMETER_NAME);
    }

    public static ServletRequestParameterFilterSource createContexted(String parameterName) {
        return new ServletRequestParameterFilterSource(SquigglyFilterSource.CONTEXT_NAME, parameterName);
    }

    public static ServletRequestParameterFilterSource createNamed(String filterName) {
        return new ServletRequestParameterFilterSource(filterName, DEFAULT_PARAMETER_NAME);
    }

    public static ServletRequestParameterFilterSource createNamed(String filterName, String parameterName) {
        return new ServletRequestParameterFilterSource(filterName parameterName);
    }
}

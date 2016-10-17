package com.github.bohnman.squiggly.web;

import com.github.bohnman.squiggly.context.provider.AbstractSquigglyContextProvider;
import com.github.bohnman.squiggly.parser.SquigglyParser;

import javax.servlet.http.HttpServletRequest;

/**
 * Custom context provider that gets the filter expression from the request.
 */
public class RequestSquigglyContextProvider extends AbstractSquigglyContextProvider {

    private String filterParam;
    private final String defaultFilter;

    public RequestSquigglyContextProvider() {
        this("fields", null);
    }

    public RequestSquigglyContextProvider(String filterParam, String defaultFilter) {
        this(new SquigglyParser(), filterParam, defaultFilter);

    }

    public RequestSquigglyContextProvider(SquigglyParser parser, String filterParam, String defaultFilter) {
        super(parser);
        this.filterParam = filterParam;
        this.defaultFilter = defaultFilter;
    }

    @Override
    protected String getFilter(Class beanClass) {
        HttpServletRequest request = getRequest();

        String filter = getFilter(request);

        if (filter == null) {
            filter = defaultFilter;
        }

        return customizeFilter(filter, request, beanClass);
    }

    @Override
    public boolean isFilteringEnabled() {
        HttpServletRequest request = getRequest();

        if (request == null) {
            return false;
        }

        String filter = getFilter(request);

        if ("**".equals(filter)) {
            return false;
        }

        if (filter != null) {
            return true;
        }

        if ("**".equals(defaultFilter)) {
            return false;
        }

        if (defaultFilter != null) {
            return true;
        }

        return false;
    }

    protected String getFilter(HttpServletRequest request) {
        return request.getParameter(filterParam);
    }

    protected HttpServletRequest getRequest() {
        return SquigglyRequestHolder.getRequest();
    }

    protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
        return filter;
    }
}

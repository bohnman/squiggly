package com.github.bohnman.squiggly.web;

import com.github.bohnman.squiggly.context.provider.AbstractSquigglyContextProvider;
import com.github.bohnman.squiggly.parser.SquigglyParser;
import com.google.common.base.MoreObjects;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

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

        FilterCache cache = FilterCache.getOrCreate(request);
        String filter = cache.get(beanClass);

        if (filter == null) {
            filter = MoreObjects.firstNonNull(getFilter(request), defaultFilter);
            filter = customizeFilter(filter, request, beanClass);
            cache.put(beanClass, filter);
        }

        return filter;
    }

    @Override
    public boolean isFilteringEnabled() {
        HttpServletRequest request = getRequest();

        if (request == null) {
            return false;
        }

        HttpServletResponse response = getResponse();


        if (response == null) {
            return false;
        }

        int status = (response instanceof StatusAwareResponse) ? ((StatusAwareResponse) response).getStatus() : HttpServletResponse.SC_OK;

        if (!isSuccessStatusCode(status)) {
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

    protected boolean isSuccessStatusCode(int status) {
        return status >= HttpServletResponse.SC_OK && status < HttpServletResponse.SC_MULTIPLE_CHOICES;
    }

    private static class FilterCache {
        @SuppressWarnings("RedundantStringConstructorCall")
        private static final String NULL = new String();
        public static final String REQUEST_KEY = FilterCache.class.getName();
        private final Map<Class, String> map = new HashMap<Class, String>();

        public static FilterCache getOrCreate(HttpServletRequest request) {
            FilterCache cache = (FilterCache) request.getAttribute(REQUEST_KEY);

            if (cache == null) {
                cache = new FilterCache();
                request.setAttribute(REQUEST_KEY, cache);
            }

            return cache;
        }

        @SuppressWarnings("StringEquality")
        public String get(Class key) {
            String value = map.get(key);

            if (value == NULL) {
                value = null;
            }

            return value;
        }

        public void put(Class key, String value) {
            if (value == null) {
                value = NULL;
            }

            map.put(key, value);
        }

        public void remove(Class key) {
            map.remove(key);
        }

        public void clear() {
            map.clear();
        }

    }

    protected String getFilter(HttpServletRequest request) {
        return request.getParameter(filterParam);
    }

    protected HttpServletRequest getRequest() {
        return SquigglyRequestHolder.getRequest();
    }

    protected HttpServletResponse getResponse() {
        return SquigglyResponseHolder.getResponse();
    }

    protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
        return customizeFilter(filter, beanClass);
    }

    protected String customizeFilter(String filter, Class beanClass) {
        return filter;
    }
}

package com.github.bohnman.squiggly.web.servlet;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.filter.support.BaseFilterContextProvider;
import com.github.bohnman.squiggly.name.support.AnyDeepName;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

/**
 * Custom context provider that gets the filter expression from a servlet request.
 */
public class RequestFilterContextProvider extends BaseFilterContextProvider {

    public static final String DEFAULT_PARAM = "fields";
    private String filterParam;
    private final String defaultFilter;

    /**
     * Constructor that looks for the filter using a the "filter" query string parameters.  This constructor does not
     * use a default filter.
     */
    public RequestFilterContextProvider() {
        this(DEFAULT_PARAM, null);
    }

    /**
     * Initialize with a given filter query string parameter.
     *
     * @param filterParam query string param name
     */
    public RequestFilterContextProvider(String filterParam) {
        this(filterParam, null);
    }

    /**
     * Initialize with a given filter query string parameter and a default filter.
     *
     * @param filterParam   query string param name
     * @param defaultFilter default filter if no query string parameter is specified
     */
    public RequestFilterContextProvider(String filterParam, @Nullable String defaultFilter) {
        this.filterParam = filterParam;
        this.defaultFilter = defaultFilter;
    }

    @Nullable
    @Override
    protected String getFilter(Class objectClass) {
        HttpServletRequest request = getRequest();

        FilterCache cache = FilterCache.getOrCreate(request);
        String filter = cache.get(objectClass);

        if (filter == null) {
            filter = CoreObjects.firstNonNull(getFilter(request), defaultFilter);
            filter = customizeFilter(filter, request, objectClass);
            cache.put(objectClass, filter);
        }

        return filter;
    }

    @Nullable
    @Override
    protected String provideFilter(Class objectClass) {
        throw new UnsupportedOperationException();
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

        return isFilteringEnabled(request, response);
    }

    protected boolean isFilteringEnabled(HttpServletRequest request, HttpServletResponse response) {
        int status = getResponseStatusCode(request, response);

        if (!isSuccessStatusCode(status)) {
            return false;
        }

        String filter = getFilter(request);

        if (AnyDeepName.ID.equals(filter)) {
            return false;
        }

        if (filter != null) {
            return true;
        }

        if (AnyDeepName.ID.equals(defaultFilter)) {
            return false;
        }

        return defaultFilter != null;
    }

    protected int getResponseStatusCode(HttpServletRequest request, HttpServletResponse response) {
        return response.getStatus();
    }

    protected boolean isSuccessStatusCode(int status) {
        return status >= HttpServletResponse.SC_OK && status < HttpServletResponse.SC_MULTIPLE_CHOICES;
    }


    /**
     * Hook method to get the filter from the request.  By default, this look for a query string parameter.
     *
     * @param request the request
     * @return filter
     */
    protected String getFilter(HttpServletRequest request) {
        return request.getParameter(filterParam);
    }

    /**
     * Hook method to get the servlet request.  By default, this uses a thread local.
     *
     * @return request
     * @see SquigglyRequestHolder#getRequest()
     */
    protected HttpServletRequest getRequest() {
        return SquigglyRequestHolder.getRequest();
    }

    /**
     * Hook method to get the servlet response.  By default, this uses a thread local.
     *
     * @return request
     * @see SquigglyResponseHolder#getResponse()
     */
    protected HttpServletResponse getResponse() {
        return SquigglyResponseHolder.getResponse();
    }

    /**
     * Hook method to change the filter.  For example, one could wrap the filter from the request in a nested filter.
     *
     * @param filter    filter string
     * @param request   web request
     * @param beanClass class of the root bean to which the filter is being applied
     * @return customer filter
     */
    protected String customizeFilter(@Nullable String filter, HttpServletRequest request, @Nullable Class beanClass) {
        return customizeFilter(filter, beanClass);
    }

    protected String customizeFilter(@Nullable String filter, @Nullable Class objectClass) {
        return filter;
    }


    private static class FilterCache {
        @SuppressWarnings("RedundantStringConstructorCall")
        private static final String NULL = new String();
        public static final String REQUEST_KEY = FilterCache.class.getName();
        private final Map<Class, String> map = new HashMap<>();

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
}

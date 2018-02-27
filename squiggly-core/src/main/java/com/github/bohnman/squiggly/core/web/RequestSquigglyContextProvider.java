package com.github.bohnman.squiggly.core.web;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.context.provider.AbstractSquigglyContextProvider;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
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

    public RequestSquigglyContextProvider(String filterParam, @Nullable String defaultFilter) {
        this.filterParam = filterParam;
        this.defaultFilter = defaultFilter;
    }

    @Nullable
    @Override
    protected String getFilter(Class beanClass) {
        HttpServletRequest request = getRequest();

        FilterCache cache = FilterCache.getOrCreate(request);
        String filter = cache.get(beanClass);

        if (filter == null) {
            filter = CoreObjects.firstNonNull(getFilter(request), defaultFilter);
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

    protected String getFilter(HttpServletRequest request) {
        return request.getParameter(filterParam);
    }

    protected HttpServletRequest getRequest() {
        return SquigglyRequestHolder.getRequest();
    }

    protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
        return filter;
    }

    public static RequestSquigglyContextProvider create() {
        return new RequestSquigglyContextProvider();
    }

    public static RequestSquigglyContextProvider create(String filterParam) {
        return new RequestSquigglyContextProvider(filterParam, null);
    }

    public static RequestSquigglyContextProvider create(String filterParam, String defaultFilter) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter);
    }

    public static RequestSquigglyContextProvider create(FilterCustomizer customizer) {
        return new RequestSquigglyContextProvider() {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.customize(filter, request, beanClass);
            }
        };
    }

    public static RequestSquigglyContextProvider create(String filterParam, FilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, null) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.customize(filter, request, beanClass);
            }
        };
    }

    public static RequestSquigglyContextProvider create(String filterParam, String defaultFilter, FilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.customize(filter, request, beanClass);
            }
        };
    }

    @FunctionalInterface
    public interface FilterCustomizer {
        String customize(String filter, HttpServletRequest request, Class beanClass);
    }
}

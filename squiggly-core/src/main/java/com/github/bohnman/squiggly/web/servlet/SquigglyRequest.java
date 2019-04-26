package com.github.bohnman.squiggly.web.servlet;

import com.github.bohnman.squiggly.filter.SquigglyFilterCustomizer;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;

/**
 * Helper class to create a {@link RequestFilterContextProvider}.
 */
public class SquigglyRequest {
    public SquigglyRequest() {
    }

    /**
     * Create a default request context provider.
     *
     * @return provider
     */
    public static RequestFilterContextProvider context() {
        return new RequestFilterContextProvider();
    }

    /**
     * Create a filter with the provided filterParam.
     *
     * @param filterParam the query string parameter name
     * @return provider.
     */
    public static RequestFilterContextProvider context(String filterParam) {
        return new RequestFilterContextProvider(filterParam, null);
    }

    /**
     * Create a filter with the provided filterParam and defaultFilter.
     *
     * @param filterParam   query string parameter
     * @param defaultFilter default filter
     * @return provider
     */
    public static RequestFilterContextProvider context(String filterParam, String defaultFilter) {
        return new RequestFilterContextProvider(filterParam, defaultFilter);
    }

    /**
     * Create a filter with the provided customizer.
     *
     * @param customizer filter customizer
     * @return provider
     */
    public static RequestFilterContextProvider context(SquigglyFilterCustomizer customizer) {
        return new RequestFilterContextProvider() {
            @Override
            protected String customizeFilter(@Nullable String filter, HttpServletRequest request, @Nullable  Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Create a filter with the provided filterParam and customizer.
     *
     * @param filterParam query string parameter
     * @param customizer  filter customizer
     * @return provider
     */
    public static RequestFilterContextProvider context(String filterParam, SquigglyFilterCustomizer customizer) {
        return new RequestFilterContextProvider(filterParam, null) {
            @Override
            protected String customizeFilter(@Nullable String filter, HttpServletRequest request, @Nullable  Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Create a filter with the provided filterParam and defaultFilter.
     *
     * @param filterParam   query string parameter
     * @param defaultFilter default filter
     * @return provider
     */
    public static RequestFilterContextProvider context(String filterParam, String defaultFilter, SquigglyFilterCustomizer customizer) {
        return new RequestFilterContextProvider(filterParam, defaultFilter) {
            @Override
            protected String customizeFilter(@Nullable String filter, HttpServletRequest request, @Nullable Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

}
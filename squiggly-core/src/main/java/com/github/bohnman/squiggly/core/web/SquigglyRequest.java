package com.github.bohnman.squiggly.core.web;

import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;

import javax.servlet.http.HttpServletRequest;

/**
 * Helper class to create a {@link RequestSquigglyContextProvider}.
 */
public class SquigglyRequest {
    public SquigglyRequest() {
    }

    /**
     * Create a default request context provider.
     *
     * @return provider
     */
    public static RequestSquigglyContextProvider context() {
        return new RequestSquigglyContextProvider();
    }

    /**
     * Create a filter with the provided filterParam.
     *
     * @param filterParam the query string parameter name
     * @return provider.
     */
    public static RequestSquigglyContextProvider context(String filterParam) {
        return new RequestSquigglyContextProvider(filterParam, null);
    }

    /**
     * Create a filter with the provided filterParam and defaultFilter.
     *
     * @param filterParam query string parameter
     * @param defaultFilter default filter
     * @return provider
     */
    public static RequestSquigglyContextProvider context(String filterParam, String defaultFilter) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter);
    }

    /**
     * Create a filter with the provided customizer.
     *
     * @param customizer filter customizer
     * @return provider
     */
    public static RequestSquigglyContextProvider context(SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider() {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Create a filter with the provided filterParam and customizer.
     *
     * @param filterParam query string parameter
     * @param customizer filter customizer
     * @return provider
     */
    public static RequestSquigglyContextProvider context(String filterParam, SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, null) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Create a filter with the provided filterParam and defaultFilter.
     *
     * @param filterParam query string parameter
     * @param defaultFilter default filter
     * @return provider
     */
    public static RequestSquigglyContextProvider context(String filterParam, String defaultFilter, SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

}
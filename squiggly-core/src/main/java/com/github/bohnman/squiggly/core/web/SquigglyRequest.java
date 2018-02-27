package com.github.bohnman.squiggly.core.web;

import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;

import javax.servlet.http.HttpServletRequest;

public class SquigglyRequest {
    public SquigglyRequest() {
    }

    public static RequestSquigglyContextProvider context() {
        return new RequestSquigglyContextProvider();
    }

    public static RequestSquigglyContextProvider context(String filterParam) {
        return new RequestSquigglyContextProvider(filterParam, null);
    }

    public static RequestSquigglyContextProvider context(String filterParam, String defaultFilter) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter);
    }

    public static RequestSquigglyContextProvider context(SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider() {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    public static RequestSquigglyContextProvider context(String filterParam, SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, null) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

    public static RequestSquigglyContextProvider context(String filterParam, String defaultFilter, SquigglyFilterCustomizer customizer) {
        return new RequestSquigglyContextProvider(filterParam, defaultFilter) {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
                return customizer.apply(filter, beanClass);
            }
        };
    }

}
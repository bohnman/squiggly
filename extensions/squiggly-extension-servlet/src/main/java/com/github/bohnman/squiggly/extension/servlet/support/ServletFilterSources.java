package com.github.bohnman.squiggly.extension.servlet.support;

import com.github.bohnman.squiggly.filter.SquigglyFilterSource;

public class ServletFilterSources {

    private ServletFilterSources() {
    }

    public static SquigglyFilterSource requestParameter() {
        return ServletRequestParameterFilterSource.createContexted();
    }

    public static SquigglyFilterSource requestParameter(String parameterName) {
        return ServletRequestParameterFilterSource.createContexted(parameterName);
    }
    public static SquigglyFilterSource requestParameter(String filterName, String parameterName) {
        return ServletRequestParameterFilterSource.createNamed(filterName, parameterName);
    }

}

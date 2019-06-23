package com.github.bohnman.squiggly.runtime;

import com.github.bohnman.squiggly.environment.SquigglyEnvironmentOld;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.match.support.DefaultExpressionMatcher;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.function.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.function.SquigglyFunctionSource;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.introspect.ObjectIntrospector;
import com.github.bohnman.squiggly.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.parse.SquigglyParser;
import com.github.bohnman.squiggly.service.SquigglyServiceSource;
import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

public interface SquigglyRuntime {

    ObjectIntrospector getObjectIntrospector();

    SquigglyEnvironmentOld getConfig();

    SquigglyConversionService getConversionService();

    SquigglyFilterContextProvider getContextProvider();

    SquigglyFilterSource getFilterRepository();

    DefaultExpressionMatcher getExpressionMatcher();

    SquigglyFunctionInvoker getFunctionInvoker();

    SquigglyFunctionSource getFunctionSource();

    SquigglyFunctionSecurity getFunctionSecurity();

    SquigglyMetrics getMetrics();

    SquigglyParser getParser();

    SquigglyServiceSource getServiceSource();

    SquigglyVariableSource getVariableResolver();
}

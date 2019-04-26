package com.github.bohnman.squiggly.engine;

import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.filter.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterRepository;
import com.github.bohnman.squiggly.filter.SquigglyNodeFilter;
import com.github.bohnman.squiggly.function.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.function.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.introspect.ObjectIntrospector;
import com.github.bohnman.squiggly.metric.support.SquigglyMetrics;
import com.github.bohnman.squiggly.parse.SquigglyParser;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;

public interface SquigglyEngine {

    <T> CoreJsonNode<T> filter(CoreJsonNode<T> node, String... filters);

    @Nullable
    <T> T find(Class<T> type);

    @SuppressWarnings("unchecked")
    @Nullable
    <T> T find(Object key, Class<T> type);

    <T> T get(Class<T> type);

    <T> T get(Object key, Class<T> type);

    ObjectIntrospector getObjectIntrospector();

    SquigglyConfig getConfig();

    SquigglyConversionService getConversionService();

    SquigglyFilterContextProvider getContextProvider();

    SquigglyFilterRepository getFilterRepository();

    SquigglyFunctionInvoker getFunctionInvoker();

    SquigglyFunctionRepository getFunctionRepository();

    SquigglyFunctionSecurity getFunctionSecurity();

    SquigglyMetrics getMetrics();

    SquigglyExpressionMatcher getExpressionMatcher();

    SquigglyNodeFilter getNodeFilter();

    SquigglyParser getParser();

    SquigglyVariableResolver getVariableResolver();
}

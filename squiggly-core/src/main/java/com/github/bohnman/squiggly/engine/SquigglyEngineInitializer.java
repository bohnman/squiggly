package com.github.bohnman.squiggly.engine;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterRepository;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

import java.util.function.Function;

public interface SquigglyEngineInitializer<I extends SquigglyEngineInitializer<I>> {

        I config(SquigglyConfig config);
        I converterRegistry(SquigglyConverterRegistry converterRegistry);
        I conversionService(Function<SquigglyConverterRegistry, SquigglyConversionService> factory);
        I converter(ConverterRecord record);
        <S, T> I converter(Class<S> source, Class<T> target, Function<S, T> converter);
        <S, T> I converter(Class<S> source, Class<T> target, Function<S, T> converter, int order);
        I filterContext(SquigglyFilterContextProvider contextProvider);
        I filterRepository(SquigglyFilterRepository filterRepository);
        I functionRepository(SquigglyFunctionRepository functionRepository);
        I functionSecurity(SquigglyFunctionSecurity functionSecurity);
        I function(SquigglyFunction<?> function);
        I functions(SquigglyFunction<?>... functions);
        I functions(Iterable<SquigglyFunction<?>> functions);
        I function(Class<?>... functionClass);
        I functions(Class<?>... classes);
        I registerDefaultConverters(boolean registerDefaultConverters);
        I registerDefaultFunctions(boolean registerDefaultFunctions);
        I savedFilter(String name, String filter);
        I serviceLocator(Function<Object, Object> serviceLocator);
        I variableResolver(SquigglyVariableResolver variableResolver);
        I variable(String name, Object value);
    

}

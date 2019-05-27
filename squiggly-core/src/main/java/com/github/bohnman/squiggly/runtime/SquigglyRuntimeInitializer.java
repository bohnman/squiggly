package com.github.bohnman.squiggly.runtime;

import com.github.bohnman.squiggly.property.SquigglyPropertySource;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.extend.SquigglyExtension;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.function.SquigglyFunctionSource;
import com.github.bohnman.squiggly.service.SquigglyServiceSource;
import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

import javax.annotation.Nullable;
import java.util.function.Function;

public interface SquigglyRuntimeInitializer<I extends SquigglyRuntimeInitializer<I>> {

    I apply(SquigglyExtension extension);

    I converter(ConverterRecord record);

    <S, T> I converter(Class<S> source, Class<T> target, Function<S, T> converter);

    <S, T> I converter(Class<S> source, Class<T> target, Function<S, T> converter, int order);

    I converterRegistry(SquigglyConverterRegistry converterRegistry);

    I conversionService(Function<SquigglyConverterRegistry, SquigglyConversionService> factory);

    I filter(String name, String filter);

    I filter(SquigglyFilterProvider filterProvider);

    I filter(SquigglyFilterSource source);

    I filterContextProvider(SquigglyFilterContextProvider contextProvider);

    I function(SquigglyFunction<?> function);

    I function(Class<?> functionClass);

    I function(SquigglyFunctionSource source);

    I functionSecurity(SquigglyFunctionSecurity functionSecurity);

    I property(String name, @Nullable String value);

    I property(SquigglyPropertySource source);

    I registerDefaultConverters(boolean registerDefaultConverters);

    I registerDefaultFunctions(boolean registerDefaultFunctions);

    <T> I service(Class<T> serviceType, T service);

    I service(String name, Object service);

    I service(SquigglyServiceSource serviceSource);

    I variable(String name, @Nullable Object value);

    I variable(SquigglyVariableSource source);

}

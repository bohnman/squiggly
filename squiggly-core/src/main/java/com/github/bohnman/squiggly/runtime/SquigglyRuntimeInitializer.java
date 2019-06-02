package com.github.bohnman.squiggly.runtime;

import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.extension.SquigglyExtension;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.function.SquigglyFunctionSource;
import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.property.SquigglyPropertySource;
import com.github.bohnman.squiggly.service.SquigglyServiceSource;
import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

import javax.annotation.Nullable;
import java.util.function.Function;

public interface SquigglyRuntimeInitializer<I extends SquigglyRuntimeInitializer<I>> {

    // region Converters

    <S, T> I addConverter(Class<S> source, Class<T> target, Function<S, T> converter);

    <S, T> I addConverter(Class<S> source, Class<T> target, int order, Function<S, T> converter);

    I converterRegistry(SquigglyConverterRegistry converterRegistry);

    I conversionService(Function<SquigglyConverterRegistry, SquigglyConversionService> factory);

    I registerDefaultConverters(boolean registerDefaultConverters);

    // endregion

    // region Extensions

    I applyExtension(SquigglyExtension extension);

    // endregion

    // region Filters

    I addFilterSource(SquigglyFilterSource source);

    I setFilter(String name, String filter);

    I filter(SquigglyFilterProvider filterProvider);

    I filterContextProvider(SquigglyFilterContextProvider contextProvider);

    // endregion

    // region Functions

    I addFunction(SquigglyFunction<?> function);

    I addFunctions(Class<?> functionClass);

    I addFunctionSource(SquigglyFunctionSource source);

    I functionSecurity(SquigglyFunctionSecurity functionSecurity);

    I registerDefaultFunctions(boolean registerDefaultFunctions);

    // endregion

    // region Matchers

    I setMatcher(SquigglyExpressionMatcher matcher);

    I addMatcherInterceptor(SquigglyExpressionMatcher interceptor);

    // endregion

    // region Properties

    I addPropertySource(SquigglyPropertySource source);

    I setProperty(String name, @Nullable String value);

    // endregion

    // region Services

    I addServiceSource(SquigglyServiceSource serviceSource);

    <T> I setService(Class<T> serviceType, T service);

    // endregion

    I setService(String name, Object service);

    // region Variables

    I addVariableSource(SquigglyVariableSource source);

    I setVariable(String name, @Nullable Object value);

    // endregion

}

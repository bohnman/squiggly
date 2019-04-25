package com.github.bohnman.squiggly.core.function;

import java.util.List;
import java.util.function.Function;

/**
 * A Squiggly Function represents a unit of execution in Squiggly giving filters the capability to perform logic.
 *
 * @param <T> the return type
 */
public interface SquigglyFunction<T> extends Function<FunctionExecutionRequest, T> {

    /**
     * The function name.
     *
     * @return name
     */
    String getName();

    /**
     * The function aliases.
     *
     * @return aliases
     */
    List<String> getAliases();

    /**
     * The function return types.
     *
     * @return return type
     */
    Class<?> getReturnType();

    /**
     * The function params.
     *
     * @return params
     */
    List<SquigglyParameter> getParameters();

    /**
     * Indicates how the function is registered.
     */
    enum RegistrationStrategy {
        /**
         * Automatically register all public static methods of a class.
         */
        AUTO,

        /**
         * Only register functions with the
         * {@link com.github.bohnman.squiggly.core.function.annotations.SquigglyFunctionMethod} annotation.
         */
        MANUAL
    }

}
package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.squiggly.core.config.SquigglyEnvironment;

import java.util.Collections;
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
     * The valid environments for the function.  By default, this is in every environment.
     *
     * @return environments
     */
    default List<SquigglyEnvironment> getEnvironments() {
        return Collections.singletonList(SquigglyEnvironment.DEFAULT);
    }

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
         * {@link com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod} annotation.
         */
        MANUAL
    }

}
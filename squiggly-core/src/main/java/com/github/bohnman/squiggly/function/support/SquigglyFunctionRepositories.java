package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionSource;

/**
 * Function repo utilities.
 */
public class SquigglyFunctionRepositories {

    private SquigglyFunctionRepositories() {
    }

    /**
     * Create a function repository from the supplied functions.
     *
     * @param functions functions
     * @return repo
     */
    public static SquigglyFunctionSource of(Iterable<SquigglyFunction<?>> functions) {
        return new MapFunctionSource(functions);
    }

    /**
     * Create a function repository from the supplied functions.
     *
     * @param functions functions
     * @return repo
     */
    public static SquigglyFunctionSource of(SquigglyFunction<?>... functions) {
        return new MapFunctionSource(functions);
    }

    /**
     * Create a function repository using all the public static methods of the supplied classes.
     *
     * @param classes the classes containing the functions
     * @return repo
     */
    @SuppressWarnings({"UnnecessaryLocalVariable"})
    public static SquigglyFunctionSource ofClasses(Class<?>... classes) {
        return ofClasses(SquigglyFunction.RegistrationStrategy.AUTO, classes);
    }

    /**
     * Create a function repository using all supplied classes and registration strategy.
     *
     * @param classes              the classes containing the functions
     * @param registrationStrategy strategy
     * @return repo
     */

    @SuppressWarnings({"UnnecessaryLocalVariable"})
    public static SquigglyFunctionSource ofClasses(SquigglyFunction.RegistrationStrategy registrationStrategy, Class<?>... classes) {
        Object[] classObjects = classes;
        return of(SquigglyFunctions.create(registrationStrategy, classObjects));
    }
}

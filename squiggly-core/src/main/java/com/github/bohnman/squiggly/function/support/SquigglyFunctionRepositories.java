package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionRepository;

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
    public static SquigglyFunctionRepository of(Iterable<SquigglyFunction<?>> functions) {
        return new MapFunctionRepository(functions);
    }

    /**
     * Create a function repository from the supplied functions.
     *
     * @param functions functions
     * @return repo
     */
    public static SquigglyFunctionRepository of(SquigglyFunction<?>... functions) {
        return new MapFunctionRepository(functions);
    }

    /**
     * Create a function repository using all the public static methods of the supplied classes.
     *
     * @param classes the classes containing the functions
     * @return repo
     */
    @SuppressWarnings({"UnnecessaryLocalVariable"})
    public static SquigglyFunctionRepository ofClasses(Class<?>... classes) {
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
    public static SquigglyFunctionRepository ofClasses(SquigglyFunction.RegistrationStrategy registrationStrategy, Class<?>... classes) {
        Object[] classObjects = classes;
        return of(SquigglyFunctions.create(registrationStrategy, classObjects));
    }
}

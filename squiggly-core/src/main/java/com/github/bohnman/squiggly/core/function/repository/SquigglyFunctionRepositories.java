package com.github.bohnman.squiggly.core.function.repository;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.SquigglyFunctions;

import java.util.List;

public class SquigglyFunctionRepositories {

    private SquigglyFunctionRepositories() {
    }

    public static SquigglyFunctionRepository of(Iterable<SquigglyFunction<?>> functions) {
        return new MapFunctionRepository(functions);
    }

    public static SquigglyFunctionRepository of(SquigglyFunction<?>... functions) {
        return new MapFunctionRepository(functions);
    }

    @SuppressWarnings({"unchecked", "UnnecessaryLocalVariable"})
    public static SquigglyFunctionRepository ofClasses(Class<?>... classes) {
        return ofClasses(SquigglyFunction.RegistrationStrategy.AUTO, classes);
    }

    @SuppressWarnings({"unchecked", "UnnecessaryLocalVariable"})
    public static SquigglyFunctionRepository ofClasses(SquigglyFunction.RegistrationStrategy registrationStrategy, Class<?>... classes) {
        Object[] classObjects = classes;
        return of((List) SquigglyFunctions.create(registrationStrategy, classObjects));
    }
}

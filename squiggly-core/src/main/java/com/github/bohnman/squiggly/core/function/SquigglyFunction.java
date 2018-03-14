package com.github.bohnman.squiggly.core.function;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public interface SquigglyFunction<T> extends Function<FunctionRequest, T> {

    String getName();

    List<String> getAliases();

    Class<?> getReturnType();

    List<SquigglyParameter> getParameters();

    default List<Environment> getEnvironments() {
        return Collections.singletonList(Environment.BASE);
    }

    enum RegistrationStrategy {
        AUTO,
        MANUAL
    }

    enum Environment {
        BASE ,
        UNSAFE
    }
}
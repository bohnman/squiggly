package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.squiggly.core.config.SquigglyEnvironment;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public interface SquigglyFunction<T> extends Function<FunctionRequest, T> {

    String getName();

    List<String> getAliases();

    Class<?> getReturnType();

    List<SquigglyParameter> getParameters();

    default List<SquigglyEnvironment> getEnvironments() {
        return Collections.singletonList(SquigglyEnvironment.DEFAULT);
    }

    enum RegistrationStrategy {
        AUTO,
        MANUAL
    }

}
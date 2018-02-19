package com.github.bohnman.squiggly.function;

import java.util.List;
import java.util.function.Function;

public interface SquigglyFunction<T> extends Function<FunctionRequest, T> {

    String getName();

    List<String> getAliases();

    enum RegistrationStrategy {
        AUTO,
        MANUAL
    }
}
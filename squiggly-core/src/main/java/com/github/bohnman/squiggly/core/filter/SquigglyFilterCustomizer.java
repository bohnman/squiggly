package com.github.bohnman.squiggly.core.filter;

import java.util.function.BiFunction;

@FunctionalInterface
public interface SquigglyFilterCustomizer extends BiFunction<String, Class<?>, String> {
}

package com.github.bohnman.squiggly.jackson.config;

import com.github.bohnman.squiggly.jackson.Squiggly;

import java.util.function.Function;

@FunctionalInterface
public interface SquigglyCustomizer extends Function<Squiggly.Builder, Squiggly.Builder> {
}

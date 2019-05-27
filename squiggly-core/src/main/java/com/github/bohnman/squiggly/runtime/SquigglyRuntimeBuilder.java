package com.github.bohnman.squiggly.runtime;

public interface SquigglyRuntimeBuilder<B extends SquigglyRuntimeInitializer<B>, R extends SquigglyRuntime> extends SquigglyRuntimeInitializer<B> {

    R build();
}

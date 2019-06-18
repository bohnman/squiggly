package com.github.bohnman.squiggly.extension;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;

@FunctionalInterface
public interface SquigglyExtension {

    void apply(SquigglyRuntimeInitializer initializer);
}

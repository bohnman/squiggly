package com.github.bohnman.squiggly.extend;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;

public interface SquigglyExtension {

    void apply(SquigglyRuntimeInitializer initializer);
}

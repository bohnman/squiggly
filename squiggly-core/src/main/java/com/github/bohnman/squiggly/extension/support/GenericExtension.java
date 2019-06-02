package com.github.bohnman.squiggly.extension.support;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;
import com.github.bohnman.squiggly.extension.SquigglyExtension;

public abstract class GenericExtension implements SquigglyExtension {

    @Override
    public final void apply(SquigglyRuntimeInitializer builder) {
        doApply(builder);
    }

    protected abstract void doApply(SquigglyRuntimeInitializer<?> builder);
}

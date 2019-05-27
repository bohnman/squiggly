package com.github.bohnman.squiggly.extend.support;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;
import com.github.bohnman.squiggly.extend.SquigglyExtension;

public abstract class GenericExtension implements SquigglyExtension {

    @Override
    public final void apply(SquigglyRuntimeInitializer builder) {
        doApply(builder);
    }

    protected abstract void doApply(SquigglyRuntimeInitializer<?> builder);
}

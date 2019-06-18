package com.github.bohnman.squiggly.joda;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;
import com.github.bohnman.squiggly.extension.support.GenericExtension;

public class JodaExtension extends GenericExtension {

    @Override
    protected void doApply(SquigglyRuntimeInitializer<?> initializer) {
        initializer.addFunctions(JodaConverters.class);
    }
}

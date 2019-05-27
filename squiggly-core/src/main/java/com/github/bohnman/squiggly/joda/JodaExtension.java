package com.github.bohnman.squiggly.joda;

import com.github.bohnman.squiggly.runtime.SquigglyRuntimeInitializer;
import com.github.bohnman.squiggly.extend.support.GenericExtension;
import com.github.bohnman.squiggly.joda.support.JodaConverters;

public class JodaExtension extends GenericExtension {

    @Override
    protected void doApply(SquigglyRuntimeInitializer<?> initializer) {
        initializer.function(JodaConverters.class);
    }
}

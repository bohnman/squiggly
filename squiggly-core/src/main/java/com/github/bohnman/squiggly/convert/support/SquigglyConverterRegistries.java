package com.github.bohnman.squiggly.convert.support;

import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;

import java.util.function.Consumer;

public class SquigglyConverterRegistries {

    public SquigglyConverterRegistries() {
    }

    public static SquigglyConverterRegistry create() {
        return new ListConverterRegistry();
    }

    public static SquigglyConverterRegistry create(Consumer<SquigglyConverterRegistry> registrar) {
        SquigglyConverterRegistry registry = create();
        registrar.accept(registry);
        return registry;
    }
}

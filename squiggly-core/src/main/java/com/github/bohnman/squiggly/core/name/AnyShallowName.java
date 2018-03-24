package com.github.bohnman.squiggly.core.name;

public class AnyShallowName implements SquigglyName {

    public static final String ID = "*";
    private static final AnyShallowName INSTANCE = new AnyShallowName();

    @Override
    public String getName() {
        return ID;
    }

    @Override
    public String getRawName() {
        return ID;
    }

    @Override
    public int match(String name) {
        return 1;
    }

    public static AnyShallowName get() {
        return INSTANCE;
    }

    @Override
    public String toString() {
        return ID;
    }
}

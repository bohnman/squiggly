package com.github.bohnman.squiggly.core.name;

public class AnyDeepName implements SquigglyName {

    public static final String ID = "**";

    private static final AnyDeepName INSTANCE = new AnyDeepName();

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
        return 0;
    }

    public static AnyDeepName get() {
        return INSTANCE;
    }
}

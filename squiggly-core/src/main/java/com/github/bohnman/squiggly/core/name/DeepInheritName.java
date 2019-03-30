package com.github.bohnman.squiggly.core.name;

public class DeepInheritName extends BaseSquigglyName {

    public static final String ID = "...";
    private static final DeepInheritName INSTANCE = new DeepInheritName();

    @Override
    public String getName() {
        return ID;
    }

    @Override
    public int getSpecificity() {
        return 0;
    }

    @Override
    public boolean matches(String name) {
        return false;
    }

    public static DeepInheritName get() {
        return INSTANCE;
    }
}

package com.github.bohnman.squiggly.core.name;

public abstract class BaseSquigglyName implements SquigglyName {

    @Override
    public String getRawName() {
        return getName();
    }

    @Override
    public String toString() {
        return getName();
    }
}

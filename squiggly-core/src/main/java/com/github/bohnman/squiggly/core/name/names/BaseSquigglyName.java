package com.github.bohnman.squiggly.core.name.names;

import com.github.bohnman.squiggly.core.name.SquigglyName;

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

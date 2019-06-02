package com.github.bohnman.squiggly.name;

import com.github.bohnman.squiggly.name.SquigglyName;

public abstract class BaseSquigglyName implements SquigglyName {

    @Override
    public String toString() {
        return getName();
    }
}

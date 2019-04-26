package com.github.bohnman.squiggly.name.support;

import com.github.bohnman.squiggly.name.SquigglyName;

public abstract class BaseSquigglyName implements SquigglyName {

    @Override
    public String toString() {
        return getName();
    }
}

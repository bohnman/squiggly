package com.github.bohnman.squiggly.name;

public abstract class BaseSquigglyName implements SquigglyName {

    @Override
    public String toString() {
        return getToken();
    }
}

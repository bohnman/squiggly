package com.github.bohnman.squiggly.core.name;

public interface SquigglyName {

    String getName();

    String getRawName();

    int match(String name);
}

package com.github.bohnman.squiggly.core.name;

public class ExactName implements SquigglyName {

    private final String name;

    public ExactName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getRawName() {
        return name;
    }

    @Override
    public int match(String name) {
        if (this.name.equals(name)) {
            return Integer.MAX_VALUE;
        }

        return -1;
    }

    @Override
    public String toString() {
        return name;
    }
}

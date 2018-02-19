package com.github.bohnman.squiggly.function;

import com.google.common.collect.ImmutableList;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

public abstract class AbstractSquigglyFunction<T> implements SquigglyFunction<T> {

    private final String name;
    private final ImmutableList<String> aliases;

    public AbstractSquigglyFunction(String name) {
        this(name, Collections.emptyList());
    }

    public AbstractSquigglyFunction(String name, String... aliases) {
        this(name, Arrays.asList(aliases));
    }

    public AbstractSquigglyFunction(String name, Iterable<String> aliases) {
        this.name = checkNotNull(name);
        this.aliases = ImmutableList.copyOf(aliases);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public List<String> getAliases() {
        return aliases;
    }
}

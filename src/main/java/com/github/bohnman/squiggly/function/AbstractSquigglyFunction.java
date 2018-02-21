package com.github.bohnman.squiggly.function;

import com.google.common.collect.ImmutableList;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;
import static java.lang.String.format;

public abstract class AbstractSquigglyFunction<T> implements SquigglyFunction<T> {

    private final String name;
    private final Class<?> returnType;
    private final List<SquigglyParameter> parameters;
    private final List<String> aliases;

    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyParameter> parameters) {
        this(name, returnType, parameters, Collections.emptyList());
    }

    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyParameter> parameters, String... aliases) {
        this(name, returnType, parameters, Arrays.asList(aliases));
    }

    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyParameter> parameters, Iterable<String> aliases) {
        this.name = checkNotNull(name);
        this.returnType = checkNotNull(returnType);

        int paramSize = parameters.size();

        for (int i = 0; i < paramSize; i++) {
            SquigglyParameter param = parameters.get(i);

            if (param.isVarArgs()) {
                if (i < (paramSize - 1)) {
                    throw new IllegalArgumentException(format("Error param [%s] of function [%s]: varargs is only supported for the last element", i, name));
                }

                if (!Object[].class.isAssignableFrom(param.getType())) {
                    throw new IllegalArgumentException(format("Error param [%s] of function [%s]: varargs must be an array type", i, name));
                }
            }
        }

        this.parameters = ImmutableList.copyOf(parameters);
        this.aliases = ImmutableList.copyOf(aliases);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Class<?> getReturnType() {
        return returnType;
    }

    @Override
    public List<SquigglyParameter> getParameters() {
        return parameters;
    }

    @Override
    public List<String> getAliases() {
        return aliases;
    }
}

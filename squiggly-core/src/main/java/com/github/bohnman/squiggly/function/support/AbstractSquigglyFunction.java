package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.core.collect.CoreLists;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionParameter;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.lang.String.format;

/**
 * Base class that provides default logic for functions.
 *
 * @param <T> result type
 */
public abstract class AbstractSquigglyFunction<T> implements SquigglyFunction<T> {

    private final String name;
    private final Class<?> returnType;
    private final List<SquigglyFunctionParameter> parameters;
    private final List<String> aliases;

    /**
     * Constructor.
     *
     * @param name       function name
     * @param returnType return type
     * @param parameters function params
     */
    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyFunctionParameter> parameters) {
        this(name, returnType, parameters, Collections.emptyList());
    }

    /**
     * Constructor.
     *
     * @param name       function name
     * @param returnType return type
     * @param parameters function params
     * @param aliases    function aliases
     */
    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyFunctionParameter> parameters, String... aliases) {
        this(name, returnType, parameters, Arrays.asList(aliases));
    }

    /**
     * Constructor.
     *
     * @param name       function name
     * @param returnType return type
     * @param parameters function params
     * @param aliases    function aliases
     */
    public AbstractSquigglyFunction(String name, Class<?> returnType, List<SquigglyFunctionParameter> parameters, Iterable<String> aliases) {
        this.name = notNull(name);
        this.returnType = notNull(returnType);

        int paramSize = parameters.size();

        for (int i = 0; i < paramSize; i++) {
            SquigglyFunctionParameter param = parameters.get(i);

            if (param.isVarArgs()) {
                if (i < (paramSize - 1)) {
                    throw new IllegalArgumentException(format("Error param [%s] of function [%s]: varargs is only supported for the last element", i, name));
                }

                if (!Object[].class.isAssignableFrom(param.getType())) {
                    throw new IllegalArgumentException(format("Error param [%s] of function [%s]: varargs must be an array type", i, name));
                }
            }
        }

        this.parameters = Collections.unmodifiableList(parameters);
        this.aliases = Collections.unmodifiableList(CoreLists.of(aliases));
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
    public List<SquigglyFunctionParameter> getParameters() {
        return parameters;
    }

    @Override
    public List<String> getAliases() {
        return aliases;
    }
}

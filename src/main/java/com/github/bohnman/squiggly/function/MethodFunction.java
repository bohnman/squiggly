package com.github.bohnman.squiggly.function;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static java.lang.String.format;

public class MethodFunction extends AbstractSquigglyFunction<Object> {

    private final Method method;
    private final Object owner;

    public MethodFunction(Method method, Object owner) {
        this(method, owner, method.getName());
    }

    public MethodFunction(Method method, Object owner, String name) {
        this(method, owner, name, Collections.emptyList());
    }

    public MethodFunction(Method method, Object owner, String name, String... aliases) {
        this(method, owner, name, Arrays.asList(aliases));
    }

    public MethodFunction(Method method, Object owner, String name, Iterable<String> aliases) {
        super(name, aliases);
        this.method = checkNotNull(method);
        this.owner = checkNotNull(owner);
        checkArgument(Modifier.isPublic(method.getModifiers()), format("Method [%s] must be public.", method));
    }

    @Override
    public Object apply(FunctionRequest request) {
        try {
            return method.invoke(owner);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(format("Error executing [%s].", method), e);
        }
    }
}

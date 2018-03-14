package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.lang.String.format;

public class MethodFunction extends AbstractSquigglyFunction<Object> {

    private final List<Environment> environments;
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
        super(name, method.getReturnType(),
                Arrays.stream(method.getParameters())
                        .map(MethodFunction::toParameter)
                        .collect(Collectors.toList()),
                aliases);
        SquigglyFunctionMethod functionAnnotation = method.getAnnotation(SquigglyFunctionMethod.class);
        this.environments = functionAnnotation == null ? Collections.singletonList(Environment.BASE) : Arrays.asList(functionAnnotation.env());
        this.method = notNull(method);
        this.owner = notNull(owner);
        isTrue(Modifier.isPublic(method.getModifiers()), format("Method [%s] must be public.", method));
    }

    @Override
    public List<Environment> getEnvironments() {
        return environments;
    }

    @Override
    public Object apply(FunctionRequest request) {
        try {
            return method.invoke(owner, request.getParameters().toArray());
        } catch (Throwable e) {
            throw new RuntimeException(format("Error executing [%s].", method), e);
        }
    }

    private static SquigglyParameter toParameter(Parameter parameter) {
        return new SquigglyParameter(parameter.getType(), parameter.isVarArgs());
    }
}

package com.github.bohnman.squiggly.function;

import com.github.bohnman.squiggly.function.annotation.SquigglyClass;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nullable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.google.common.base.Preconditions.checkArgument;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;

public class SquigglyFunctions {

    private SquigglyFunctions() {
    }

    public static <T> SquigglyFunction<T> create(String name, Function<FunctionRequest, T> function) {
        return new AbstractSquigglyFunction<T>(name) {
            @Override
            public T apply(FunctionRequest request) {
                return function.apply(request);
            }
        };
    }

    public static <T> SquigglyFunction<T> create(String name, Function<FunctionRequest, T> function, String... aliases) {
        return new AbstractSquigglyFunction<T>(name, aliases) {
            @Override
            public T apply(FunctionRequest request) {
                return function.apply(request);
            }
        };
    }

    public static <T> SquigglyFunction<T> create(String name, Function<FunctionRequest, T> function, Iterable<String> aliases) {
        return new AbstractSquigglyFunction<T>(name, aliases) {
            @Override
            public T apply(FunctionRequest request) {
                return function.apply(request);
            }
        };
    }

    public static SquigglyFunction<Object> create(Method method) {
        return create(method, null, (Iterable<String>) null);
    }


    public static SquigglyFunction<Object> create(Method method, String name) {
        return create(method, name, (Iterable<String>) null);
    }

    public static SquigglyFunction<Object> create(Method method, String name, String... aliases) {
        return create(method, name, Arrays.asList(aliases));
    }

    public static SquigglyFunction<Object> create(Method method, @Nullable String name, @Nullable Iterable<String> aliases) {
        return create(method, method.getDeclaringClass(), name, aliases);
    }

    public static SquigglyFunction<Object> create(Method method, Object owner) {
        return create(method, owner, null);
    }

    public static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name) {
        return create(method, owner, name, (Iterable<String>) null);
    }

    public static SquigglyFunction<Object> create(Method method, Object owner, String name, String... aliases) {
        return create(method, owner, name, Arrays.asList(aliases));
    }

    public static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases) {
        return create(method, owner, name, aliases, owner.getClass().getAnnotation(SquigglyClass.class));
    }

    private static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyClass classAnnotation) {
        return create(method, owner, name, aliases, classAnnotation, method.getAnnotation(com.github.bohnman.squiggly.function.annotation.SquigglyFunction.class));
    }

    private static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyClass classAnnotation, @Nullable com.github.bohnman.squiggly.function.annotation.SquigglyFunction functionAnnotation) {
        if (functionAnnotation != null && functionAnnotation.ignore()) {
            throw new IllegalArgumentException(format("Method [%s] is marked as ignored.", method));
        }

        checkArgument(Modifier.isPublic(method.getModifiers()), format("Method [%s] must be public.", method));

        if (owner instanceof Class) {
            checkArgument(Modifier.isStatic(method.getModifiers()), format("Method [%s] must be static", method));
        } else {
            checkArgument(!Modifier.isStatic(method.getModifiers()), format("Method [%s] must not be static", method));
        }

        String prefix = classAnnotation == null ? "" : classAnnotation.prefix();

        String functionName = prefix + method.getName();

        if (name != null) {
            functionName = name;
        } else if (functionAnnotation != null && StringUtils.isNotEmpty(functionAnnotation.value())) {
            functionName = prefix + functionAnnotation.value();
        }

        Iterable<String> functionAliases = Collections.emptyList();

        if (aliases != null) {
            functionAliases = aliases;
        } else if (functionAliases != null && functionAnnotation.aliases() != null & functionAnnotation.aliases().length > 0) {
            functionAliases = Stream.of(functionAnnotation.aliases())
                    .map(alias -> prefix + alias)
                    .collect(toList());
        }

        return new MethodFunction(method, owner, functionName, functionAliases);
    }


    public static List<SquigglyFunction<Object>> create(Object owner) {
        return create(owner, SquigglyFunction.RegistrationStrategy.AUTO);
    }

    public static List<SquigglyFunction<Object>> create(Object owner, SquigglyFunction.RegistrationStrategy registrationStrategy) {
        boolean ownerStatic = owner instanceof Class;

        SquigglyClass classAnnotation = owner.getClass().getAnnotation(SquigglyClass.class);

        return Arrays.stream(owner.getClass().getDeclaredMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .filter(method -> ownerStatic && Modifier.isStatic(method.getModifiers()))
                .map(method -> {
                    com.github.bohnman.squiggly.function.annotation.SquigglyFunction functonAnnotation = method.getAnnotation(com.github.bohnman.squiggly.function.annotation.SquigglyFunction.class);

                    if (functonAnnotation == null && SquigglyFunction.RegistrationStrategy.MANUAL == registrationStrategy) {
                        return null;
                    }

                    if (functonAnnotation != null && functonAnnotation.ignore()) {
                        return null;
                    }

                    return create(method, owner, null, null, classAnnotation, functonAnnotation);
                })
                .filter(Objects::nonNull)
                .collect(toList());
    }
}


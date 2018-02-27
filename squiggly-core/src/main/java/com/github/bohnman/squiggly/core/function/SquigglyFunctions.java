package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyClass;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

import javax.annotation.Nullable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;

public class SquigglyFunctions {

    private SquigglyFunctions() {
    }

    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionRequest, T> function) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionRequest.class).build())) {
            @Override
            public T apply(FunctionRequest request) {
                return function.apply(request);
            }
        };
    }

    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionRequest, T> function, String... aliases) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionRequest.class).build()), aliases) {

            @Override
            public T apply(FunctionRequest request) {
                return function.apply(request);
            }
        };
    }

    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionRequest, T> function, Iterable<String> aliases) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionRequest.class).build()), aliases) {
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
        Class<?> ownerClass = (owner instanceof Class) ? (Class) owner : owner.getClass();
        return create(method, owner, name, aliases, ownerClass.getAnnotation(SquigglyClass.class));
    }

    private static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyClass classAnnotation) {
        return create(method, owner, name, aliases, classAnnotation, method.getAnnotation(SquigglyMethod.class));
    }

    private static SquigglyFunction<Object> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyClass classAnnotation, @Nullable SquigglyMethod functionAnnotation) {
        if (functionAnnotation != null && functionAnnotation.ignore()) {
            throw new IllegalArgumentException(format("Method [%s] is marked as ignored.", method));
        }

        isTrue(Modifier.isPublic(method.getModifiers()), format("Method [%s] must be public.", method));

        if (owner instanceof Class) {
            isTrue(Modifier.isStatic(method.getModifiers()), format("Method [%s] must be static", method));
        } else {
            isTrue(!Modifier.isStatic(method.getModifiers()), format("Method [%s] must not be static", method));
        }

        String prefix = classAnnotation == null ? "" : classAnnotation.prefix();

        String functionName = prefix + method.getName();

        if (name != null) {
            functionName = name;
        } else if (functionAnnotation != null && CoreStrings.isNotEmpty(functionAnnotation.value())) {
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
        return create(SquigglyFunction.RegistrationStrategy.AUTO, owner);
    }

    public static List<SquigglyFunction<Object>> create(SquigglyFunction.RegistrationStrategy registrationStrategy, Object... owners) {
        return createInternal(registrationStrategy, owners).collect(toList());
    }

    private static Stream<SquigglyFunction<Object>> createInternal(SquigglyFunction.RegistrationStrategy registrationStrategy, Object... owners) {
        Map<Class<?>, Class<?>> processed = new IdentityHashMap<>();

        return Arrays.stream(owners)
                .flatMap(owner -> createSingleInternal(registrationStrategy, owner,  processed));

    }

    private static Stream<SquigglyFunction<Object>> createSingleInternal(SquigglyFunction.RegistrationStrategy registrationStrategy, Object owner, Map<Class<?>, Class<?>> processed) {
        boolean ownerStatic = owner instanceof Class;
        Class<?> ownerClass = (owner instanceof Class) ? (Class) owner : owner.getClass();

        if (processed.putIfAbsent(ownerClass, ownerClass) != null) {
            return Stream.empty();
        }

        SquigglyClass classAnnotation = ownerClass.getAnnotation(SquigglyClass.class);

        Stream<SquigglyFunction<Object>> stream = Arrays.stream(ownerClass.getDeclaredMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .filter(method -> ownerStatic && Modifier.isStatic(method.getModifiers()))
                .map(method -> {
                    SquigglyMethod functonAnnotation = method.getAnnotation(SquigglyMethod.class);

                    if (functonAnnotation == null && SquigglyFunction.RegistrationStrategy.MANUAL == registrationStrategy) {
                        return null;
                    }

                    if (functonAnnotation != null && functonAnnotation.ignore()) {
                        return null;
                    }

                    return create(method, owner, null, null, classAnnotation, functonAnnotation);
                })
                .filter(Objects::nonNull);


        if (classAnnotation != null) {
            for (Class<?> includeClass : classAnnotation.include()) {
                stream = Stream.concat(stream, createSingleInternal(registrationStrategy, includeClass, processed));
            }
        }

        return stream;


    }
}


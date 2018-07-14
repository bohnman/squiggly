package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionClass;
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod;

import javax.annotation.Nullable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;

/**
 * Function utility methods.
 */
public class SquigglyFunctions {

    private SquigglyFunctions() {
    }

    /**
     * Create a function.
     *
     * @param name       function name
     * @param returnType return type class
     * @param function   java function
     * @param <T>        return type
     * @return squiggly function
     */
    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionExecutionRequest, T> function) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionExecutionRequest.class).build())) {
            @Override
            public T apply(FunctionExecutionRequest request) {
                return function.apply(request);
            }
        };
    }

    /**
     * Create a function.
     *
     * @param name       function name
     * @param returnType return type class
     * @param function   java function
     * @param aliases    function aliases
     * @param <T>        return type
     * @return squiggly function
     */
    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionExecutionRequest, T> function, String... aliases) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionExecutionRequest.class).build()), aliases) {

            @Override
            public T apply(FunctionExecutionRequest request) {
                return function.apply(request);
            }
        };
    }

    /**
     * Create a function.
     *
     * @param name       function name
     * @param returnType return type class
     * @param function   java function
     * @param aliases    function aliases
     * @param <T>        return type
     * @return squiggly function
     */
    public static <T> SquigglyFunction<T> create(String name, Class<T> returnType, Function<FunctionExecutionRequest, T> function, Iterable<String> aliases) {
        return new AbstractSquigglyFunction<T>(name, returnType, Collections.singletonList(SquigglyParameter.builder(FunctionExecutionRequest.class).build()), aliases) {
            @Override
            public T apply(FunctionExecutionRequest request) {
                return function.apply(request);
            }
        };
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method) {
        return create(method, null, (Iterable<String>) null);
    }


    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param name function name
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, String name) {
        return create(method, name, (Iterable<String>) null);
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param name function name
     * @param aliases function aliases
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, String name, String... aliases) {
        return create(method, name, Arrays.asList(aliases));
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param name function name
     * @param aliases function aliases
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, @Nullable String name, @Nullable Iterable<String> aliases) {
        return create(method, method.getDeclaringClass(), name, aliases);
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param owner method owner
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, Object owner) {
        return create(method, owner, null);
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param owner method owner
     * @param name function name
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, Object owner, @Nullable String name) {
        return create(method, owner, name, (Iterable<String>) null);
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param owner method owner
     * @param name function name
     * @param aliases function aliases
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, Object owner, String name, String... aliases) {
        return create(method, owner, name, Arrays.asList(aliases));
    }

    /**
     * Create a function from a method.
     *
     * @param method java method
     * @param owner method owner
     * @param name function name
     * @param aliases function aliases
     * @return squiggly function
     */
    public static SquigglyFunction<?> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases) {
        Class<?> ownerClass = (owner instanceof Class) ? (Class) owner : owner.getClass();
        return create(method, owner, name, aliases, ownerClass.getAnnotation(SquigglyFunctionClass.class));
    }

    private static SquigglyFunction<?> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyFunctionClass classAnnotation) {
        return create(method, owner, name, aliases, classAnnotation, method.getAnnotation(SquigglyFunctionMethod.class));
    }

    private static SquigglyFunction<?> create(Method method, Object owner, @Nullable String name, @Nullable Iterable<String> aliases, @Nullable SquigglyFunctionClass classAnnotation, @Nullable SquigglyFunctionMethod functionAnnotation) {
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
        } else if (functionAnnotation != null && functionAnnotation.aliases() != null & functionAnnotation.aliases().length > 0) {
            functionAliases = Stream.of(functionAnnotation.aliases())
                    .map(alias -> prefix + alias)
                    .collect(toList());
        }

        return new MethodFunction(method, owner, functionName, functionAliases);
    }


    public static List<SquigglyFunction<?>> create(Object... owners) {
        return createInternal(owners).collect(toList());
    }

    private static Stream<SquigglyFunction<?>> createInternal(Object... owners) {
        Map<Class<?>, Class<?>> processed = new IdentityHashMap<>();

        return Arrays.stream(owners)
                .flatMap(owner -> createSingleInternal(owner, processed));

    }

    @SuppressWarnings("unchecked")
    private static Stream<SquigglyFunction<?>> createSingleInternal(Object owner, Map<Class<?>, Class<?>> processed) {
        boolean ownerStatic = owner instanceof Class;
        Class<?> ownerClass = (owner instanceof Class) ? (Class) owner : owner.getClass();

        if (processed.putIfAbsent(ownerClass, ownerClass) != null) {
            return Stream.empty();
        }

        SquigglyFunctionClass classAnnotation = ownerClass.getAnnotation(SquigglyFunctionClass.class);
        SquigglyFunction.RegistrationStrategy registrationStrategy = (classAnnotation == null) ? SquigglyFunction.RegistrationStrategy.AUTO : classAnnotation.strategy();

        Stream<? extends SquigglyFunction<?>> stream = Arrays.stream(ownerClass.getDeclaredMethods())
                .filter(method -> Modifier.isPublic(method.getModifiers()))
                .filter(method -> ownerStatic && Modifier.isStatic(method.getModifiers()))
                .map(method -> {
                    SquigglyFunctionMethod functonAnnotation = method.getAnnotation(SquigglyFunctionMethod.class);

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
                stream = Stream.concat(stream, createSingleInternal(includeClass, processed));
            }
        }

        return (Stream) stream;
    }
}


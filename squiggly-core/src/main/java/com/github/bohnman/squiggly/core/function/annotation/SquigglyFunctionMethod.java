package com.github.bohnman.squiggly.core.function.annotation;

import com.github.bohnman.squiggly.core.config.SquigglyEnvironment;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates a method should be registered as a function with squiggly.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface SquigglyFunctionMethod {

    /**
     * Primary name of the function.  If empty, the name with based upon the name of the method.
     *
     * @return value
     */
    String value() default "";

    /**
     * Alternative names for the function.
     *
     * @return aliases
     */
    String[] aliases() default "";

    /**
     * Indicates to exclude this method from registration.  This is useful when
     * {@link com.github.bohnman.squiggly.core.function.SquigglyFunction.RegistrationStrategy} is set to AUTO.
     *
     * @return ignore
     */
    boolean ignore() default false;

    /**
     * Indicates which environments this function is to be registered.
     *
     * @return environment
     */
    SquigglyEnvironment[] env() default SquigglyEnvironment.DEFAULT;
}


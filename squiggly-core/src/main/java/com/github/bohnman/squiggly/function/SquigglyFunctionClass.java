package com.github.bohnman.squiggly.function;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that the class contains functions to use with Squiggly.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface SquigglyFunctionClass {

    /**
     * Apply the supplied prefix to all of the functions in the class.
     *
     * @return prefix
     */
    String prefix() default "";

    /**
     * Include other classes as functions, along with this class.
     *
     * @return classes
     */
    Class<?>[] include() default {};

    /**
     * Specify how functions get registered.  AUTO means to register all public static methods of the class.
     * Manual means to register only those methods that have the {@link SquigglyFunctionMethod} annotation.
     * The default is AUTO.
     *
     * @return strategy
     */
    SquigglyFunction.RegistrationStrategy strategy() default SquigglyFunction.RegistrationStrategy.AUTO;
}

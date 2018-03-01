package com.github.bohnman.squiggly.core.function.annotation;

import com.github.bohnman.squiggly.core.function.SquigglyFunction;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface SquigglyClass {

    String prefix() default "";

    Class<?>[] include() default {};

    SquigglyFunction.RegistrationStrategy strategy() default SquigglyFunction.RegistrationStrategy.AUTO;
}

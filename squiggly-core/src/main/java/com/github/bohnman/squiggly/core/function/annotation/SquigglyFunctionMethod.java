package com.github.bohnman.squiggly.core.function.annotation;

import com.github.bohnman.squiggly.core.config.SquigglyEnvironment;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface SquigglyFunctionMethod {
    String value() default "";

    String[] aliases() default "";

    boolean ignore() default false;

    SquigglyEnvironment[] env() default SquigglyEnvironment.DEFAULT;
}


package com.github.bohnman.squiggly.function.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface SquigglyFunction {
    String value() default "";
    String[] aliases() default "";
    boolean ignore() default false;
}
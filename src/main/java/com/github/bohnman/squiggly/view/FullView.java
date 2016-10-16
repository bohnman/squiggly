package com.github.bohnman.squiggly.view;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Convenience annotation that indicates the field belongs to the "full" view.
 */
@Target(FIELD)
@Retention(RUNTIME)
@Documented
@PropertyView({"full"})
public @interface FullView {
}

package com.github.bohnman.squiggly.view;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Convenience annotation indicating that a field belong to the "base" view.  This annotation is generally
 * not needed because a non-annotated implictly belongs to the "base" view.
 */
@Target({FIELD, METHOD})
@Retention(RUNTIME)
@Documented
@PropertyView(PropertyView.BASE_VIEW)
public @interface BaseView {
}

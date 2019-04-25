package com.github.bohnman.squiggly.core.view.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Annotation that marks that a field belongs to 1 ore more views.  This annotation can also be placed on other
 * annotations in order to use your own annotations.
 */
@Target({ANNOTATION_TYPE, FIELD, METHOD})
@Retention(RUNTIME)
@Documented
public @interface PropertyView {
    String BASE_VIEW = "base";
    String FULL_VIEW = "full";

    /**
     * The views that the field will belong to.
     *
     * @return views
     */
    String[] value();
}

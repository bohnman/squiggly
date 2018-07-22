package com.github.bohnman.squiggly.core.function.security;

/**
 * Security API for property access in function arguments.  This is used to prevent a client request access to
 * properties that have been excluded from serialization.
 */
public interface SquigglyFunctionSecurity {

    SquigglyFunctionSecurity ALWAYS_ALLOW = (name, type) -> true;
    SquigglyFunctionSecurity ALWAYS_DENY = (name, type) -> false;

    /**
     * Determines is the property for the given class.
     *
     * @param key property name
     * @param type owner type
     * @return true if viewable.
     */
    boolean isPropertyViewable(Object key, Class type);
}

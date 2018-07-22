package com.github.bohnman.squiggly.core.function.security;

public interface SquigglyFunctionSecurity {

    SquigglyFunctionSecurity ALWAYS_ALLOW = (name, pojoClass) -> true;
    SquigglyFunctionSecurity ALWAYS_DENY = (name, pojoClass) -> false;

    boolean isPropertyViewable(Object key, Class pojoClass);
}

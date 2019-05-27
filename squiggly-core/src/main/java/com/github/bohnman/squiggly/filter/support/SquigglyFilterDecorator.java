package com.github.bohnman.squiggly.filter.support;

public interface SquigglyFilterDecorator {

    String apply(String filter, Class<?> objectClass);
}

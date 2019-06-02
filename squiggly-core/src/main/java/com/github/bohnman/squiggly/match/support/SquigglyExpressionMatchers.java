package com.github.bohnman.squiggly.match.support;

import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;

import java.util.Arrays;
import java.util.List;

public class SquigglyExpressionMatchers {

    private SquigglyExpressionMatchers() {
    }

    public static SquigglyExpressionMatcher matchObjectClass(Class<?> objectClass) {
        return ObjectClassExpressionMatcher.create(objectClass);
    }

    public static SquigglyExpressionMatcher matchObjectClassAndProperty(Class<?> objectClass, String property) {
        return ObjectClassExpressionMatcher.create(objectClass, property);
    }

    public static SquigglyExpressionMatcher of(SquigglyExpressionMatcher... matchers) {
        return CompositeExpressionMatcher.create(Arrays.asList(matchers));
    }

    public static SquigglyExpressionMatcher of(List<SquigglyExpressionMatcher> matchers) {
        return CompositeExpressionMatcher.create(matchers);
    }
}

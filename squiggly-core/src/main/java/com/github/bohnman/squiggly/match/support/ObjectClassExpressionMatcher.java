package com.github.bohnman.squiggly.match.support;

import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.path.SquigglyObjectPath;

import javax.annotation.Nullable;

public class ObjectClassExpressionMatcher implements SquigglyExpressionMatcher {

    private final Class<?> objectClass;

    @Nullable
    private final String property;

    private ObjectClassExpressionMatcher(Class<?> objectClass, @Nullable String property) {
        this.objectClass = objectClass;
        this.property = property;
    }

    @Override
    public ExpressionNode match(SquigglyObjectPath path, String filter, ExpressionNode expression) {
        if (objectClass.equals(path.getType()) && (property == null || property.equals(path.getName()))) {
            return SquigglyExpressionMatcher.ALWAYS_MATCH;
        }

        return expression;
    }

    public static ObjectClassExpressionMatcher create(Class<?> objectClass) {
        return new ObjectClassExpressionMatcher(objectClass, null);
    }

    public static ObjectClassExpressionMatcher create(Class<?> objectClass, String property) {
        return new ObjectClassExpressionMatcher(objectClass, property);
    }
}

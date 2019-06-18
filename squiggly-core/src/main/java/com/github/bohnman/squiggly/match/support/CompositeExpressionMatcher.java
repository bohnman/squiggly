package com.github.bohnman.squiggly.match.support;

import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.path.SquigglyObjectPath;

import java.util.List;

import static java.util.Objects.requireNonNull;

public class CompositeExpressionMatcher implements SquigglyExpressionMatcher {

    private final List<SquigglyExpressionMatcher> matchers;

    private CompositeExpressionMatcher(List<SquigglyExpressionMatcher> matchers) {
        this.matchers = requireNonNull(matchers);
    }

    @Override
    public ExpressionNode match(SquigglyObjectPath path, String filter, ExpressionNode expression) {
        for (SquigglyExpressionMatcher matcher : matchers) {
            expression = matcher.match(path, filter, expression);

            if (expression == SquigglyExpressionMatcher.ALWAYS_MATCH) {
                return expression;
            }

            if (expression == SquigglyExpressionMatcher.NEVER_MATCH) {
                return expression;
            }
        }

        return expression;
    }

    public static CompositeExpressionMatcher create(List<SquigglyExpressionMatcher> matchers) {
        return new CompositeExpressionMatcher(matchers);
    }
}

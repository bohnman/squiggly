package com.github.bohnman.squiggly.match;

import com.github.bohnman.squiggly.name.SquigglyNames;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.node.StatementNode;
import com.github.bohnman.squiggly.path.SquigglyObjectPath;

public interface SquigglyExpressionMatcher {
    /**
     * Indicate to never match the path.
     */
    ExpressionNode NEVER_MATCH = ExpressionNode.createNamed(SquigglyNames.neverMatch());
    /**
     * Indicate to always match the path.
     */
    ExpressionNode ALWAYS_MATCH = ExpressionNode.createNamed(SquigglyNames.anyDeep());

    ExpressionNode match(SquigglyObjectPath path, String filter, ExpressionNode expression);

    default ExpressionNode match(SquigglyObjectPath path, String filter, StatementNode statement) {
        return match(path, filter, statement.getRoot());
    }
}

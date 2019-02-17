package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

public class StatementNode extends BaseSquigglyNode {

    private final ExpressionNode rootExpression;

    public StatementNode(ParseContext context, ExpressionNode rootExpression) {
        super(context, SquigglyNodeType.STATEMENT);
        this.rootExpression = rootExpression;
    }

    public ExpressionNode getRootExpression() {
        return rootExpression;
    }
}

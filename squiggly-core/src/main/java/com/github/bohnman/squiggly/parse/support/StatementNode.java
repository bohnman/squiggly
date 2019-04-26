package com.github.bohnman.squiggly.parse.support;

import com.github.bohnman.squiggly.parse.ParseContext;

public class StatementNode extends BaseSquigglyNode {

    private final ExpressionNode root;

    public StatementNode(ParseContext context, ExpressionNode root) {
        super(context);
        this.root = root;
    }

    public ExpressionNode getRoot() {
        return root;
    }
}

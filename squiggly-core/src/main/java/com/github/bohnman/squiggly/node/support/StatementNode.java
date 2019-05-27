package com.github.bohnman.squiggly.node.support;

import com.github.bohnman.squiggly.parse.SquigglyParseContext;

public class StatementNode extends BaseSquigglyNode {

    private final ExpressionNode root;

    public StatementNode(SquigglyParseContext context, ExpressionNode root) {
        super(context);
        this.root = root;
    }

    public ExpressionNode getRoot() {
        return root;
    }
}

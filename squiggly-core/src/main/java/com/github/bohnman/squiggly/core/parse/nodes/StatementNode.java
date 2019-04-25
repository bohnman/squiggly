package com.github.bohnman.squiggly.core.parse.nodes;

import com.github.bohnman.squiggly.core.parse.ParseContext;

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

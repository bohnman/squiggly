package com.github.bohnman.squiggly.node;

import javax.annotation.Nullable;

public class StatementNode extends BaseNode {

    private final ExpressionNode root;

    StatementNode(SquigglyNodeOrigin origin, ExpressionNode root) {
        super(origin);
        this.root = root;
    }

    public ExpressionNode getRoot() {
        return root;
    }

    public static class Builder extends BaseNodeBuilder<StatementNode> {

        @Nullable
        private ExpressionNode.Builder root;

        Builder(SquigglyNodeOrigin origin) {
            super(origin);
        }

        @Nullable
        public ExpressionNode.Builder getRoot() {
            return root;
        }

        public Builder root(ExpressionNode.Builder root) {
            this.root = root;
            return this;
        }

        public StatementNode build() {
            return new StatementNode(getOrigin(), root.build());
        }
    }
}

package com.github.bohnman.squiggly.node;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class FilterNode extends BaseNode {

    public static final FilterNode EMPTY = new FilterNode(SquigglyNodeOrigin.EMPTY, Collections.emptyList());

    private final List<StatementNode> statements;

    FilterNode(SquigglyNodeOrigin origin, List<StatementNode> statements) {
        super(origin);
        this.statements = statements;
    }

    public List<StatementNode> getStatements() {
        return statements;
    }

    public static class Builder extends BaseNodeBuilder<FilterNode> {

        final ArrayList<StatementNode.Builder> statements;

        Builder(SquigglyNodeOrigin origin) {
            super(origin);
            this.statements = new ArrayList<>();
        }

        Builder(SquigglyNodeOrigin origin, int statementsSize) {
            super(origin);
            this.statements = new ArrayList<>(statementsSize);
        }

        public List<StatementNode.Builder> getStatements() {
            return statements;
        }

        public Builder statement(StatementNode.Builder statement) {
            this.statements.add(statement);
            return this;
        }

        public FilterNode build() {
            List<StatementNode> statements = new ArrayList<>(this.statements.size());

            for (StatementNode.Builder statement : this.statements) {
                statements.add(statement.build());
            }

            return new FilterNode(getOrigin(), statements);
        }
    }
}

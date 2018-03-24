package com.github.bohnman.squiggly.core.parse.node;

import java.util.List;

public class IfNode {

    private final List<IfClause> ifClauses;
    private final ArgumentNode elseClause;

    public IfNode(List<IfClause> ifClauses, ArgumentNode elseClause) {
        this.ifClauses = ifClauses;
        this.elseClause = elseClause;
    }

    public List<IfClause> getIfClauses() {
        return ifClauses;
    }

    public ArgumentNode getElseClause() {
        return elseClause;
    }

    public static class IfClause {
        private final ArgumentNode condition;
        private final ArgumentNode value;

        public IfClause(ArgumentNode condition, ArgumentNode value) {
            this.condition = condition;
            this.value = value;
        }

        public ArgumentNode getCondition() {
            return condition;
        }

        public ArgumentNode getValue() {
            return value;
        }
    }
}

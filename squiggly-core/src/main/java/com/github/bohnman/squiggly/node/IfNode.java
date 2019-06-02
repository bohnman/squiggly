package com.github.bohnman.squiggly.node;

import javax.annotation.Nullable;
import java.util.List;

/**
 * Represents an if statement
 */
public class IfNode extends BaseNode {

    private final List<IfClause> ifClauses;
    private final ArgumentNode elseClause;

    /**
     * Constructor.
     *
     * @param ifClauses  test conditions
     * @param elseClause else logic
     */
    IfNode(SquigglyNodeOrigin origin, @Nullable ArgumentNode elseClause, List<IfClause> ifClauses) {
        super(origin);
        this.ifClauses = ifClauses;
        this.elseClause = elseClause;
    }

    /**
     * Get the test clauses
     *
     * @return if clauses
     */
    public List<IfClause> getIfClauses() {
        return ifClauses;
    }

    /**
     * Get the otherwise clause
     *
     * @return else clause
     */
    @Nullable
    public ArgumentNode getElseClause() {
        return elseClause;
    }

    /**
     * An if clause contains a test condition and a logic node if the test passes.
     */
    public static class IfClause {
        private final ArgumentNode condition;
        private final ArgumentNode value;

        /**
         * Constructor.
         *
         * @param condition test condition
         * @param value     logic to execute if argument is true
         */
        IfClause(ArgumentNode condition, ArgumentNode value) {
            this.condition = condition;
            this.value = value;
        }

        /**
         * Get the test condition.
         *
         * @return test condition
         */
        public ArgumentNode getCondition() {
            return condition;
        }

        /**
         * Get the logic node.
         *
         * @return logic node
         */
        public ArgumentNode getValue() {
            return value;
        }
    }
}

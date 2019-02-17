package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

import java.util.List;

/**
 * Represents an if statement
 */
public class IfNode extends BaseSquigglyNode {

    private final List<IfClause> ifClauses;
    private final ArgumentNode elseClause;

    /**
     * Constructor.
     *
     * @param ifClauses  test conditions
     * @param elseClause else logic
     */
    public IfNode(ParseContext parseContext, List<IfClause> ifClauses, ArgumentNode elseClause) {
        super(parseContext, SquigglyNodeType.IF);
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
        public IfClause(ArgumentNode condition, ArgumentNode value) {
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

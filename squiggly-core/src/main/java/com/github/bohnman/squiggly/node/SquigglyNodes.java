package com.github.bohnman.squiggly.node;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;

public class SquigglyNodes {
    private SquigglyNodes() {
    }

    public static ArgumentNode.Builder argument(SquigglyNodeOrigin origin) {
        return new ArgumentNode.Builder(origin);
    }

    public static ExpressionNode.Builder expression(SquigglyNodeOrigin origin) {
        return new ExpressionNode.Builder(origin);
    }

    public static FilterNode.Builder filter(SquigglyNodeOrigin origin) {
        return new FilterNode.Builder(origin);
    }

    public static FilterNode.Builder filter(SquigglyNodeOrigin origin, int statementCapacity) {
        return new FilterNode.Builder(origin, statementCapacity);
    }


    public static FilterNode filter(SquigglyNodeOrigin origin, List<StatementNode> statements) {
        return new FilterNode(origin, statements);
    }

    public static FunctionNode.Builder function(SquigglyNodeOrigin origin) {
        return new FunctionNode.Builder(origin);
    }

    public static IfNode.IfClause ifClause(SquigglyNodeOrigin origin, ArgumentNode condition, ArgumentNode value) {
        return new IfNode.IfClause(condition, value);
    }

    public static IfNode ifNode(SquigglyNodeOrigin origin, @Nullable ArgumentNode elseClause, List<IfNode.IfClause> ifClauses) {
        return new IfNode(origin, elseClause, ifClauses);
    }

    public static IfNode ifNode(SquigglyNodeOrigin origin, @Nullable ArgumentNode elseClause, IfNode.IfClause... ifClauses) {
        return new IfNode(origin, elseClause, Arrays.asList(ifClauses));
    }

    public static IfNode ifNode(SquigglyNodeOrigin origin, List<IfNode.IfClause> ifClauses) {
        return new IfNode(origin, null, ifClauses);
    }

    public static IfNode ifNode(SquigglyNodeOrigin origin, IfNode.IfClause... ifClauses) {
        return new IfNode(origin, null, Arrays.asList(ifClauses));
    }

    public static IntRangeNode intRangeExclusive(SquigglyNodeOrigin origin, ArgumentNode.Builder start, @Nullable  ArgumentNode.Builder end) {
        return new IntRangeNode(origin, start, end, true);
    }

    public static IntRangeNode intRangeInclusive(SquigglyNodeOrigin origin, ArgumentNode.Builder start, ArgumentNode.Builder end) {
        return new IntRangeNode(origin, start, end, false);
    }

    public static LambdaNode lambda(SquigglyNodeOrigin origin, FunctionNode body, String... arguments) {
        return new LambdaNode(origin, body, Arrays.asList(arguments));
    }

    public static LambdaNode lambda(SquigglyNodeOrigin origin, FunctionNode body, List<String> arguments) {
        return new LambdaNode(origin, body, arguments);
    }

    public static StatementNode.Builder statement(SquigglyNodeOrigin origin) {
        return new StatementNode.Builder(origin);
    }

    public static StatementNode statement(SquigglyNodeOrigin origin, ExpressionNode root) {
        return new StatementNode(origin, root);
    }
}

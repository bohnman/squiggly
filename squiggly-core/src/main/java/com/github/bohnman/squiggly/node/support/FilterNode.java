package com.github.bohnman.squiggly.node.support;

import com.github.bohnman.squiggly.parse.SquigglyParseContext;

import java.util.Collections;
import java.util.List;

public class FilterNode extends BaseSquigglyNode {

    public static final FilterNode EMPTY = new FilterNode(SquigglyParseContext.EMPTY, Collections.emptyList());

    private final List<StatementNode> statements;

    public FilterNode(SquigglyParseContext context, List<StatementNode> statements) {
        super(context);
        this.statements = statements;
    }

    public List<StatementNode> getStatements() {
        return statements;
    }
}

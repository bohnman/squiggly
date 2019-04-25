package com.github.bohnman.squiggly.core.parse.nodes;

import com.github.bohnman.squiggly.core.parse.ParseContext;

import java.util.Collections;
import java.util.List;

public class FilterNode extends BaseSquigglyNode {

    public static final FilterNode EMPTY = new FilterNode(ParseContext.EMPTY, Collections.emptyList());

    private final List<StatementNode> statements;

    public FilterNode(ParseContext context, List<StatementNode> statements) {
        super(context);
        this.statements = statements;
    }

    public List<StatementNode> getStatements() {
        return statements;
    }
}

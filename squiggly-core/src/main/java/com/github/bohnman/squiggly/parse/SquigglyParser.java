package com.github.bohnman.squiggly.parse;

import com.github.bohnman.squiggly.node.FilterNode;

/**
 * The parser takes a filter expression and compiles it to an Abstract Syntax Tree (AST).  In this parser's case, the
 * tree doesn't have a root node but rather just returns top level nodes.
 */
public interface SquigglyParser {

    String SELF_REFERENCE = "$";
    String PARENT_REFERENCE = "$$";


    FilterNode parseNodeFilter(String filter);

    FilterNode parsePropertyFilter(String filter);
}

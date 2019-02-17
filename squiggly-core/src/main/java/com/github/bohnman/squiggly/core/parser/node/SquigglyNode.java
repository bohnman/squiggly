package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

public interface SquigglyNode {

    ParseContext getContext();

    SquigglyNodeType getType();
}

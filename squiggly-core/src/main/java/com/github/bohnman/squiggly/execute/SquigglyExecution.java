package com.github.bohnman.squiggly.execute;

import com.github.bohnman.squiggly.node.FilterNode;

public interface SquigglyExecution extends SquigglyExecutionContext {

    SquigglyExecution cancel();

    boolean isCancelled();

    SquigglyExecution withFilter(String filter);

    SquigglyExecution withFilterNode(FilterNode filterNode);


}

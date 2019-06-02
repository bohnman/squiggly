package com.github.bohnman.squiggly.exec;

import com.github.bohnman.squiggly.node.support.FilterNode;
import com.github.bohnman.squiggly.path.SquigglyObjectPath;

public interface SquigglyExecution {

    SquigglyExecution cancel();
    boolean isCancelled();

    String getFilter();
    FilterNode getFilterNode();

    SquigglyObjectPath getPath();

    SquigglyExecution withFilter(String filter);
    SquigglyExecution withFilterNode(FilterNode filterNode);

    default String getName() {
        return getPath().getName();
    }

    default Class<?> getType() {
        return getPath().getType();
    }
}

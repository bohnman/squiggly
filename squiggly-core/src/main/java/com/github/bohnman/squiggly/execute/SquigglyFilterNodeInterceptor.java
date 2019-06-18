package com.github.bohnman.squiggly.execute;

import com.github.bohnman.squiggly.node.FilterNode;

import javax.annotation.Nullable;

@FunctionalInterface
public interface SquigglyFilterNodeInterceptor {

    @Nullable
    FilterNode applyFilterNode(SquigglyFilterContext context, FilterNode node);
}

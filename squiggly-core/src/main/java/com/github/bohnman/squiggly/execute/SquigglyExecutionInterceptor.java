package com.github.bohnman.squiggly.execute;

import com.github.bohnman.squiggly.node.FilterNode;

import javax.annotation.Nullable;

public interface SquigglyExecutionInterceptor extends SquigglyFilterInterceptor, SquigglyFilterNodeInterceptor {

    @Override
    @Nullable
    default String applyFilter(SquigglyPathContext context, String filter) {
        return filter;
    }

    @Override
    @Nullable
    default FilterNode applyFilterNode(SquigglyFilterContext context, FilterNode node) {
        return node;
    }
}

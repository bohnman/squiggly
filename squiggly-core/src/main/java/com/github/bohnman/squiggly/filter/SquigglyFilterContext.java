package com.github.bohnman.squiggly.filter;

import com.github.bohnman.squiggly.node.support.FilterNode;

/**
 * A squiggly context provides parsing and filtering information for parsing.
 */
public interface SquigglyFilterContext {

    /**
     * Get the parsed node.
     *
     * @return nodes
     */
    FilterNode getFilterNode();

    /**
     * Get the filter expression.
     *
     * @return filter expression
     */
    String getFilter();
}

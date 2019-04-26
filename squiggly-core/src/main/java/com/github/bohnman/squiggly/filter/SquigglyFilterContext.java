package com.github.bohnman.squiggly.filter;

import com.github.bohnman.squiggly.parse.support.FilterNode;

/**
 * A squiggly context provides parsing and filtering information for parsing.
 */
public interface SquigglyFilterContext {

    /**
     * Get the parsed node.
     *
     * @return nodes
     */
    FilterNode getParsedFilter();

    /**
     * Get the filter expression.
     *
     * @return filter expression
     */
    String getFilter();
}

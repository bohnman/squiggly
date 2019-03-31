package com.github.bohnman.squiggly.core.context;

import com.github.bohnman.squiggly.core.parser.node.ExpressionNode;
import com.github.bohnman.squiggly.core.parser.node.FilterNode;

/**
 * A squiggly context provides parsing and filtering information for parsing.
 */
public interface SquigglyContext {

    /**
     * Get the top-level bean class being filtered.
     *
     * @return bean class
     */
    Class getBeanClass();

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

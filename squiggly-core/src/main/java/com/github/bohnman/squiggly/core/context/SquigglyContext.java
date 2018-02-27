package com.github.bohnman.squiggly.core.context;

import com.github.bohnman.squiggly.core.parser.SquigglyNode;

import java.util.List;

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
     * Get the parsed nodes.
     *
     * @return nodes
     */
    List<SquigglyNode> getNodes();

    /**
     * Get the filter expression.
     *
     * @return filter expression
     */
    String getFilter();
}

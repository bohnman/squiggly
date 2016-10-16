package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.annotation.JsonFilter;
import net.jcip.annotations.ThreadSafe;

/**
 * Jackson mixin that register the filter id for the @{@link SquigglyPropertyFilter}.
 */
@ThreadSafe
@JsonFilter(SquigglyPropertyFilter.FILTER_ID)
public class SquigglyPropertyFilterMixin {
}

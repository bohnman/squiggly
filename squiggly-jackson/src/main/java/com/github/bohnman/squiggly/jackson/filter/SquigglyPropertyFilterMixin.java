package com.github.bohnman.squiggly.jackson.filter;

import com.fasterxml.jackson.annotation.JsonFilter;

import javax.annotation.concurrent.ThreadSafe;

/**
 * Jackson mixin that register the filter id for the @{@link SquigglyPropertyFilter}.
 */
@ThreadSafe
@JsonFilter(SquigglyPropertyFilter.FILTER_ID)
public class SquigglyPropertyFilterMixin {
}

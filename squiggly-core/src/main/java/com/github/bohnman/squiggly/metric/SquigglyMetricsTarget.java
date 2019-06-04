package com.github.bohnman.squiggly.metric;

@FunctionalInterface
public interface SquigglyMetricsTarget {

    void setMetric(String name, Object value);
}

package com.github.bohnman.squiggly.metric;

@FunctionalInterface
public interface SquigglyMetricsProducer {

    void sendMetrics(SquigglyMetricsTarget target);
}

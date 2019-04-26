package com.github.bohnman.squiggly.metric.support;

import com.github.bohnman.squiggly.metric.SquigglyMetricsSource;

import javax.annotation.concurrent.ThreadSafe;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Provides API for obtaining various metrics in the squiggly libraries, such as cache statistics.
 */
@ThreadSafe
public class SquigglyMetrics {

    private final CompositeMetricsSource metricsSource = new CompositeMetricsSource();

    /**
     * Adds a new source to the metrics.
     *
     * @param source the source
     */
    public void add(SquigglyMetricsSource source) {
        metricsSource.add(source);
    }

    /**
     * Gets the metrics as a map whose keys are the metric name and whose values are the metric values.
     *
     * @return map
     */
    public SortedMap<String, Object> asMap() {
        SortedMap<String, Object> metrics = new TreeMap<>();
        metricsSource.applyMetrics(metrics);
        return metrics;
    }

    @Override
    public String toString() {
        return asMap().toString();
    }
}

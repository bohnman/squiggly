package com.github.bohnman.squiggly.metric;

import com.github.bohnman.squiggly.metric.source.CompositeSquigglyMetricsSource;
import com.github.bohnman.squiggly.metric.source.SquigglyMetricsSource;
import com.google.common.collect.Maps;
import net.jcip.annotations.ThreadSafe;

import java.util.SortedMap;

/**
 * Provides API for obtaining various metrics in the squiggly libraries, such as cache statistics.
 */
@ThreadSafe
public class SquigglyMetrics {

    private final CompositeSquigglyMetricsSource metricsSource = new CompositeSquigglyMetricsSource();

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
        SortedMap<String, Object> metrics = Maps.newTreeMap();
        metricsSource.applyMetrics(metrics);
        return metrics;
    }
}

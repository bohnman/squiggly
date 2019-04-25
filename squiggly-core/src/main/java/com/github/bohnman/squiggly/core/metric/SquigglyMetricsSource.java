package com.github.bohnman.squiggly.core.metric;

import java.util.Map;

/**
 * Represents a source that can supply metrics.  Implementations should be thread safe.
 */
public interface SquigglyMetricsSource {

    /**
     * Apply the source's metrics to the provided map.
     *
     * @param map the map to add the metrics to
     */
    void applyMetrics(Map<String, Object> map);
}

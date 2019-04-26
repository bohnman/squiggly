package com.github.bohnman.squiggly.metric.support;

import com.github.bohnman.squiggly.metric.SquigglyMetricsSource;

import javax.annotation.concurrent.ThreadSafe;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A source that pull metrics from multiple other sources.
 */
@ThreadSafe
public class CompositeMetricsSource implements SquigglyMetricsSource {

    private final List<SquigglyMetricsSource> sources = new CopyOnWriteArrayList<>();

    /**
     * Register a source with this composite.
     *
     * @param source a source
     */
    public void add(SquigglyMetricsSource source) {
        sources.add(source);
    }

    @Override
    public void applyMetrics(Map<String, Object> map) {
        for (SquigglyMetricsSource source : sources) {
            source.applyMetrics(map);
        }
    }
}

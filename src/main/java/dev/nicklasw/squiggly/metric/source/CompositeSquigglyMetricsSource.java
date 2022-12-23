package dev.nicklasw.squiggly.metric.source;

import com.google.common.collect.ImmutableList;

import java.util.Map;

/**
 * A source that pull metrics from multiple other sources.
 */
public class CompositeSquigglyMetricsSource implements SquigglyMetricsSource {

    private final ImmutableList<SquigglyMetricsSource> sources;

    public CompositeSquigglyMetricsSource(SquigglyMetricsSource... sources) {
        this.sources = ImmutableList.copyOf(sources);
    }

    @Override
    public void applyMetrics(Map<String, Object> map) {
        for (SquigglyMetricsSource source : sources) {
            source.applyMetrics(map);
        }
    }
}

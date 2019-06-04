package com.github.bohnman.squiggly.metric;

import com.github.bohnman.core.cache.CoreCache;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.util.Objects.requireNonNull;

public class SquigglyMetrics {

    private SquigglyMetrics() {
    }

    public static SquigglyMetricsProducer compositeProducer(SquigglyMetricsProducer... producers) {
        return new CompositeMetricsProducer(Arrays.asList(producers));
    }

    public static SquigglyMetricsProducer compositeProducer(List<SquigglyMetricsProducer> producers) {
        return new CompositeMetricsProducer(producers);
    }

    public static SquigglyMetricsProducer coreCacheProducer(String prefix, CoreCache<?, ?> cache) {
        return new CoreCacheMetricsProducer(prefix, cache);
    }

    public static void send(SquigglyMetricsProducer producer, Map<String, Object> target) {
        producer.sendMetrics(new MapMetricsTarget(target));
    }

    /**
     * A source that pull metrics from multiple other sources.
     */
    @ThreadSafe
    private static class CompositeMetricsProducer implements SquigglyMetricsProducer {

        private final List<SquigglyMetricsProducer> producers;

        public CompositeMetricsProducer(List<SquigglyMetricsProducer> producers) {
            this.producers = producers;
        }

        @Override
        public void sendMetrics(SquigglyMetricsTarget target) {
            for (SquigglyMetricsProducer producer : producers) {
                producer.sendMetrics(target);
            }
        }
    }

    /**
     * A source that provides metrics from a Core {@link CoreCache}.
     */
    @ThreadSafe
    private static class CoreCacheMetricsProducer implements SquigglyMetricsProducer {

        private final String prefix;
        private final CoreCache cache;

        /**
         * Constructor.
         *
         * @param prefix the prefix the use with each stats key.
         * @param cache  the cache to hold the metrics
         */
        public CoreCacheMetricsProducer(String prefix, CoreCache cache) {
            notNull(prefix);
            notNull(cache);
            this.prefix = prefix;
            this.cache = cache;
        }

        @Override
        public void sendMetrics(SquigglyMetricsTarget target) {
            CoreCache.CacheStats stats = cache.stats();
            target.setMetric(prefix + "evictions", stats.getEvictions());
            target.setMetric(prefix + "hits", stats.getHits());
            target.setMetric(prefix + "misses", stats.getMisses());
        }
    }

    private static class MapMetricsTarget implements SquigglyMetricsTarget {

        private final Map<String, Object> map;

        public MapMetricsTarget(Map<String, Object> map) {
            this.map = requireNonNull(map);
        }

        @Override
        public void setMetric(String name, Object value) {
            map.put(name, value);
        }
    }
}

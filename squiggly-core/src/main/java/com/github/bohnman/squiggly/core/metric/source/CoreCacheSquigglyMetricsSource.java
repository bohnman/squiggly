package com.github.bohnman.squiggly.core.metric.source;

import com.github.bohnman.core.cache.CoreCache;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Map;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A source that provides metrics from a Core {@link CoreCache}.
 */
@ThreadSafe
public class CoreCacheSquigglyMetricsSource implements SquigglyMetricsSource {

    private final String prefix;
    private final CoreCache cache;

    /**
     * Constructor.
     *
     * @param prefix the prefix the use with each stats key.
     * @param cache the cache to hold the metrics
     */
    public CoreCacheSquigglyMetricsSource(String prefix, CoreCache cache) {
        notNull(prefix);
        notNull(cache);
        this.prefix = prefix;
        this.cache = cache;
    }

    @Override
    public void applyMetrics(Map<String, Object> map) {
        CoreCache.CacheStats stats = cache.stats();
        map.put(prefix + "evictions", stats.getEvictions());
        map.put(prefix + "hits", stats.getHits());
        map.put(prefix + "misses", stats.getMisses());
    }
}

package com.github.bohnman.squiggly.core.metric.source;

import com.github.bohnman.core.cache.Cache;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Map;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A source that provides metrics from a Core {@link Cache}.
 */
@ThreadSafe
public class CoreCacheSquigglyMetricsSource implements SquigglyMetricsSource {

    private final String prefix;
    private final Cache cache;

    public CoreCacheSquigglyMetricsSource(String prefix, Cache cache) {
        notNull(prefix);
        notNull(cache);
        this.prefix = prefix;
        this.cache = cache;
    }

    @Override
    public void applyMetrics(Map<String, Object> map) {
        Cache.CacheStats stats = cache.stats();
        map.put(prefix + "evections", stats.getEvictions());
        map.put(prefix + "hits", stats.getHits());
        map.put(prefix + "misses", stats.getMisses());
    }
}

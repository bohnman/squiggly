package dev.nicklasw.squiggly.metric.source;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheStats;
import net.jcip.annotations.ThreadSafe;

import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * A source that provides metrics from a Guava {@link Cache}.
 */
@ThreadSafe
public class GuavaCacheSquigglyMetricsSource implements SquigglyMetricsSource {

    private final String prefix;
    private final Cache cache;

    public GuavaCacheSquigglyMetricsSource(String prefix, Cache cache) {
        checkNotNull(prefix);
        checkNotNull(cache);
        this.prefix = prefix;
        this.cache = cache;
    }

    @Override
    public void applyMetrics(Map<String, Object> map) {
        CacheStats stats = cache.stats();
        map.put(prefix + "averageLoadPenalty", stats.averageLoadPenalty());
        map.put(prefix + "evictionCount", stats.evictionCount());
        map.put(prefix + "hitCount", stats.hitCount());
        map.put(prefix + "hitRate", stats.hitRate());
        map.put(prefix + "hitCount", stats.hitCount());
        map.put(prefix + "loadExceptionCount", stats.loadExceptionCount());
        map.put(prefix + "loadExceptionRate", stats.loadExceptionRate());
        map.put(prefix + "loadSuccessCount", stats.loadSuccessCount());
        map.put(prefix + "missCount", stats.missCount());
        map.put(prefix + "missRate", stats.missRate());
        map.put(prefix + "requestCount", stats.requestCount());
        map.put(prefix + "totalLoadTime", stats.totalLoadTime());
    }
}

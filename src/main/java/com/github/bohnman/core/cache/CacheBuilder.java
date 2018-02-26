package com.github.bohnman.core.cache;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.ToLongBiFunction;

public class CacheBuilder<K, V> {
    private long maximumWeight = -1;
    private long expireAfterAccessNanos = -1;
    private long expireAfterWriteNanos = -1;
    private ToLongBiFunction<K, V> weigher;
    private RemovalListener<K, V> removalListener;

    public static <K, V> CacheBuilder<K, V> builder() {
        return new CacheBuilder<>();
    }

    private CacheBuilder() {
    }

    public CacheBuilder<K, V> setMaximumWeight(long maximumWeight) {
        if (maximumWeight < 0) {
            throw new IllegalArgumentException("maximumWeight < 0");
        }
        this.maximumWeight = maximumWeight;
        return this;
    }

    /**
     * Sets the amount of time before an entry in the cache expires after it was last accessed.
     *
     */
    public CacheBuilder<K, V> setExpireAfterAccess(long value, TimeUnit timeUnit) {
        Objects.requireNonNull(timeUnit);
        final long expireAfterAccessNanos = timeUnit.toNanos(value);
        if (expireAfterAccessNanos <= 0) {
            throw new IllegalArgumentException("expireAfterAccess <= 0");
        }
        this.expireAfterAccessNanos = expireAfterAccessNanos;
        return this;
    }

    /**
     * Sets the amount of time before an entry in the cache expires after it was written.
     *
     */
    public CacheBuilder<K, V> setExpireAfterWrite(long value, TimeUnit timeUnit) {
        Objects.requireNonNull(timeUnit);
        final long expireAfterWriteNanos = timeUnit.toNanos(value);
        if (expireAfterWriteNanos <= 0) {
            throw new IllegalArgumentException("expireAfterWrite <= 0");
        }
        this.expireAfterWriteNanos = expireAfterWriteNanos;
        return this;
    }

    public CacheBuilder<? extends K, ? extends  V> setWeigher(ToLongBiFunction<K, V> weigher) {
        Objects.requireNonNull(weigher);
        this.weigher = weigher;
        return this;
    }

    public CacheBuilder<K, V> setRemovalListener(RemovalListener<K, V> removalListener) {
        Objects.requireNonNull(removalListener);
        this.removalListener = removalListener;
        return this;
    }

    public static CacheBuilder<Object, Object> from(CacheBuilderSpec spec) {
        return spec.toCacheBuilder();
    }

    public static CacheBuilder<Object, Object> from(String spec) {
        return from(CacheBuilderSpec.parse(spec));
    }

    @SuppressWarnings("unchecked")
    public <K1 extends K, V1 extends V> Cache<K1, V1> build() {
        Cache<K1, V1> cache = new Cache<>();
        if (maximumWeight != -1) {
            cache.setMaximumWeight(maximumWeight);
        }
        if (expireAfterAccessNanos != -1) {
            cache.setExpireAfterAccessNanos(expireAfterAccessNanos);
        }
        if (expireAfterWriteNanos != -1) {
            cache.setExpireAfterWriteNanos(expireAfterWriteNanos);
        }
        if (weigher != null) {
            cache.setWeigher((ToLongBiFunction) weigher);
        }
        if (removalListener != null) {
            cache.setRemovalListener((RemovalListener) removalListener);
        }
        return cache;
    }
}

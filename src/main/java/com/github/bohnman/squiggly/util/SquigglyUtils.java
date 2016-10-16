package com.github.bohnman.squiggly.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.cache.CacheBuilder;
import net.jcip.annotations.ThreadSafe;

/**
 * Provides various convenience methods.
 */
@ThreadSafe
public class SquigglyUtils {

    private SquigglyUtils() {
    }


    /**
     * Takes an object and converts it to a string.
     *
     * @param mapper the object mapper
     * @param object the object to convert
     * @return json string
     */
    public static String stringify(ObjectMapper mapper, Object object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * Creates a new @{@link CacheBuilder}, setting the max size if >= 0.
     *
     * @param maxSize max size of the cache
     * @param <K> key
     * @param <V> value
     * @return builder
     */
    @SuppressWarnings("unchecked")
    public static <K, V> CacheBuilder<K, V> cacheBuilderWithMaxSize(int maxSize) {
        CacheBuilder<K, V> builder = (CacheBuilder<K, V>) CacheBuilder.newBuilder();

        if (maxSize >= 0) {
            builder = builder.maximumSize(maxSize);
        }

        return builder;
    }
}

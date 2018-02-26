package com.github.bohnman.core.cache;

@FunctionalInterface
public interface CacheLoader<K, V> {
    V load(K key) throws Exception;
}
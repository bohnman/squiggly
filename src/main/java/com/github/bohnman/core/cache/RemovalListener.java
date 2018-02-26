package com.github.bohnman.core.cache;

@FunctionalInterface
public interface RemovalListener<K, V> {
    void onRemoval(RemovalNotification<K, V> notification);
}

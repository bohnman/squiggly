package com.github.bohnman.core.concurrent;

import java.util.concurrent.locks.Lock;

public class ReleasableLock implements AutoCloseable {
    private final Lock lock;


    public ReleasableLock(Lock lock) {
        this.lock = lock;
    }

    @Override
    public void close() {
        lock.unlock();
    }


    public ReleasableLock acquire() {
        lock.lock();
        return this;
    }

}
package com.github.bohnman.core.collect;

import javax.annotation.Nullable;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class CoreIterators {

    public CoreIterators() {
    }

    @Nullable
    public static <T> T getNext(Iterator<? extends T> iterator, @Nullable T defaultValue) {
        return iterator.hasNext() ? iterator.next() : defaultValue;
    }

    public static <T> T getLast(Iterator<? extends T> iterator, T defaultValue) {
        return iterator.hasNext() ? getLast(iterator) : defaultValue;
    }

    /**
     * Advances {@code iterator} to the end, returning the last element.
     *
     * @return the last element of {@code iterator}
     * @throws NoSuchElementException if the iterator is empty
     */
    public static <T> T getLast(Iterator<T> iterator) {
        while (true) {
            T current = iterator.next();
            if (!iterator.hasNext()) {
                return current;
            }
        }
    }

    public static int size(Iterator<?> iterator) {
        long count = 0L;
        while (iterator.hasNext()) {
            iterator.next();
            count++;
        }
        return saturatedCast(count);
    }

    public static int saturatedCast(long value) {
        if (value > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
        }
        if (value < Integer.MIN_VALUE) {
            return Integer.MIN_VALUE;
        }
        return (int) value;
    }
}

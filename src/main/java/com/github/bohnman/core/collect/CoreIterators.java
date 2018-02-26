package com.github.bohnman.core.collect;

import com.github.bohnman.core.lang.CoreAssert;

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

    public static <T> T get(Iterator<T> iterator, int position) {
        CoreAssert.isTrue(position >= 0);
        int skipped = advance(iterator, position);
        if (!iterator.hasNext()) {
            throw new IndexOutOfBoundsException(
                    "position ("
                            + position
                            + ") must be less than the number of elements that remained ("
                            + skipped
                            + ")");
        }
        return iterator.next();
    }

    public static int advance(Iterator<?> iterator, int numberToAdvance) {
        CoreAssert.notNull(iterator);
        CoreAssert.isTrue(numberToAdvance >= 0, "numberToAdvance must be nonnegative");

        int i;
        for (i = 0; i < numberToAdvance && iterator.hasNext(); i++) {
            iterator.next();
        }
        return i;
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

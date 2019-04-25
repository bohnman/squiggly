package com.github.bohnman.squiggly.core.function.valuehandlers;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;
import com.github.bohnman.squiggly.core.function.ValueHandler;

import java.util.Collections;

/**
 * Base handler that returns a collection.
 *
 * @param <T>
 */
public abstract class BaseCollectionValueHandler<T> extends ValueHandler<T> {

    public BaseCollectionValueHandler(Object... arguments) {
        super(arguments);
    }

    /**
     * By default, return an empty list.
     *
     * @return list
     */
    @Override
    protected T handleNull() {
        return handleList(Collections.emptyList());
    }

    /**
     * By default return a singleton list.
     *
     * @param value the object
     * @return list
     */
    @Override
    protected T handleObject(Object value) {
        return handleList(Collections.singletonList(value));
    }

    @Override
    protected abstract T handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper);
}

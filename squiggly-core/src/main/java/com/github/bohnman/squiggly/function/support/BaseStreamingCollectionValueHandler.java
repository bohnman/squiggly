package com.github.bohnman.squiggly.function.support;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;

import java.util.stream.Stream;

/**
 * Base class for dealing with collection streams.
 *
 * @param <T> type
 */
public abstract class BaseStreamingCollectionValueHandler<T> extends BaseCollectionValueHandler<T> {

    public BaseStreamingCollectionValueHandler(Object... arguments) {
        super(arguments);
    }

    @Override
    protected T handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
        return handleStream(wrapper, createStream(wrapper));
    }

    /**
     * Create a stream from the given wrapper.
     *
     * @param wrapper the indexed collection wrapper
     * @return steram
     */
    protected abstract Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper);

    /**
     * Handle the stream.
     *
     * @param wrapper the indexed collection wrapper
     * @param stream  the stream
     * @return type
     */
    protected abstract T handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream);
}

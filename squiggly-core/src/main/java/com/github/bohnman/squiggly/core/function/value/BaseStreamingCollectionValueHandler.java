package com.github.bohnman.squiggly.core.function.value;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;

import java.util.stream.Stream;

public abstract class BaseStreamingCollectionValueHandler<T> extends BaseCollectionValueHandler<T> {

    public BaseStreamingCollectionValueHandler(Object... arguments) {
        super(arguments);
    }

    @Override
    protected T handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
        return handleStream(wrapper, createStream(wrapper));
    }

    protected abstract Stream<Object> createStream(CoreIndexedIterableWrapper<Object, ?> wrapper);

    protected abstract T handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream);
}

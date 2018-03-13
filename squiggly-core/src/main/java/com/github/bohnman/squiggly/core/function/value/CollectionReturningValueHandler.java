package com.github.bohnman.squiggly.core.function.value;

import com.github.bohnman.core.collect.CoreIndexedIterableWrapper;

import java.util.stream.Stream;

public abstract class CollectionReturningValueHandler extends BaseStreamingCollectionValueHandler<Object> {

    public CollectionReturningValueHandler(Object... arguments) {
        super(arguments);
    }

    @Override
    protected Object handleStream(CoreIndexedIterableWrapper<Object, ?> wrapper, Stream<Object> stream) {
        return wrapper.collect(stream);
    }
}

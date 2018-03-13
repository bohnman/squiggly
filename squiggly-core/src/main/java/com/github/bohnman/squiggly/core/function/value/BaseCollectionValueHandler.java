package com.github.bohnman.squiggly.core.function.value;

import java.util.Collections;

public abstract class BaseCollectionValueHandler<T> extends ValueHandler<T> {

    public BaseCollectionValueHandler(Object... arguments) {
        super(arguments);
    }

    @Override
    protected T handleNull() {
        return handleList(Collections.emptyList());
    }


    @Override
    protected T handleObject(Object value) {
        return handleList(Collections.singletonList(value));
    }
}

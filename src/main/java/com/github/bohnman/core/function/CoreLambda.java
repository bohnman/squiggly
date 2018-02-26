package com.github.bohnman.core.function;

import com.github.bohnman.core.lang.array.CoreArrays;

import javax.annotation.Nullable;

public interface CoreLambda extends FunctionPredicateBridge<Object, Object> {

    @Nullable
    Object invoke(Object... arguments);

    @Override
    @Nullable
    default Object apply(@Nullable Object arguments) {
        if (arguments == null) {
            return invoke(new Object[]{null});
        }

        if (arguments instanceof Object[]) {
            return invoke((Object[]) arguments);
        }

        if (arguments.getClass().isArray()) {
            invoke(CoreArrays.wrap(arguments).toArray());
        }

        return invoke(arguments);
    }

}

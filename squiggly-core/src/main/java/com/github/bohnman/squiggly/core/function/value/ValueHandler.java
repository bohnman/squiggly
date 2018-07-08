package com.github.bohnman.squiggly.core.function.value;

import com.github.bohnman.core.collect.*;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.range.CoreIntRange;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

@SuppressWarnings("unchecked")
public abstract class ValueHandler<T> {

    protected final Object[] arguments;

    public ValueHandler(Object... arguments) {
        this.arguments = arguments;
    }

    public T handle(Object value) {
        if (value == null) {
            return handleNull();
        }

        if (value.getClass().isArray()) {
            return handleArray(value);
        }

        if (value instanceof Boolean) {
            return handleBoolean((Boolean) value);
        }

        if (value instanceof Character) {
            return handleCharacter((Character) value);
        }

        if (value instanceof CoreLambda) {
            return handleLambda((CoreLambda) value);
        }

        if (value instanceof CoreProperty) {
            return handleProperty((CoreProperty) value);
        }

        if (value instanceof Function) {
            Function<Object, Object> function = (Function) value;
            return handleFunction(function);
        }

        if (value instanceof Predicate) {
            Predicate<Object> predicate = (Predicate) value;
            return handlePredicate(predicate);
        }

        if (value instanceof CoreIntRange) {
            return handleIntRange((CoreIntRange) value);
        }

        if (value instanceof Iterable) {
            Iterable<Object> iterable = (Iterable) value;
            return handleIterable(iterable);
        }

        if (value instanceof Map) {
            Map<Object, Object> map = (Map) value;
            return handleMap(map);
        }

        if (value instanceof Number) {
            return handleNumber((Number) value);
        }

        if (value instanceof String) {
            return handleString((String) value);
        }

        return handleObject(value);
    }

    protected T handleNull() {
        return null;
    }

    protected T handleArray(Object array) {
        return handleArrayWrapper(CoreArrays.wrap(array));
    }

    protected T handleArrayWrapper(CoreArrayWrapper wrapper) {
        return handleIndexedCollectionWrapper(wrapper);
    }

    protected T handleBoolean(Boolean bool) {
        return handleObject(bool);
    }

    protected T handleCharacter(Character character) {
        return handleString(Character.toString(character));
    }

    protected T handleFunction(Function<Object, Object> function) {
        if (arguments.length == 0) {
            return handleNull();
        }

        return handle(function.apply(arguments[0]));
    }

    protected T handleIntRange(CoreIntRange range) {
        return handleObject(range);
    }

    protected T handleIterable(Iterable<Object> iterable) {
        return handleList(CoreLists.of(iterable));
    }

    protected T handleLambda(CoreLambda lambda) {
        return handle(lambda.invoke(arguments));
    }

    protected T handleList(List<Object> list) {
        return handleListWrapper(new CoreListWrapper<>(list));
    }

    protected T handleListWrapper(CoreListWrapper<Object> wrapper) {
        return handleIndexedCollectionWrapper(wrapper);
    }

    protected T handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
        return handleObject(wrapper.getValue());
    }

    protected T handleMap(Map<Object, Object> map) {
        return handleObject(map);
    }

    protected T handleNumber(Number number) {
        return handleObject(number);
    }

    protected T handlePredicate(Predicate<Object> predicate) {
        if (arguments.length == 0) {
            return handleNull();
        }

        return handle(predicate.test(arguments[0]));
    }

    protected T handleProperty(CoreProperty property) {
        return handleFunction(property);
    }

    protected T handleString(String string) {
        return handleObject(string);
    }

    protected abstract T handleObject(Object value);
}

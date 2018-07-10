package com.github.bohnman.squiggly.core.function.value;

import com.github.bohnman.core.collect.*;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.range.CoreIntRange;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A base class used by functions to handle generic input values.  This class provide several hook methods
 * for handling various types.
 *
 * @param <T> the return type
 */
@SuppressWarnings("unchecked")
public abstract class ValueHandler<T> {

    protected final Object[] arguments;

    /**
     * Function args.
     *
     * @param arguments args
     */
    public ValueHandler(Object... arguments) {
        this.arguments = arguments;
    }

    /**
     * Handle the generic value
     *
     * @param value the input
     * @return type
     */
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

    /**
     * Hook method to handle a null value.  By default, it just returns null.
     *
     * @return type
     */
    protected T handleNull() {
        return null;
    }

    /**
     * Hook method to handle array values.  By default, it just delegates to
     * {@link #handleArrayWrapper(CoreArrayWrapper)}.
     *
     * @param array the array
     * @return type
     */
    protected T handleArray(Object array) {
        return handleArrayWrapper(CoreArrays.wrap(array));
    }

    /**
     * Hook method to handle an array-like structure.
     *
     * @param wrapper the array wrapper
     * @return type
     */
    protected T handleArrayWrapper(CoreArrayWrapper wrapper) {
        return handleIndexedCollectionWrapper(wrapper);
    }

    /**
     * Hook method to handle a boolean value.  By default, it just delegates to {@link #handleObject(Object)}.
     *
     * @param bool the boolean value
     * @return type
     */
    protected T handleBoolean(Boolean bool) {
        return handleObject(bool);
    }


    /**
     * Hook method to handle a character value.  By default, it just delegates to {@link #handleString(String)}.
     *
     * @param character the char value
     * @return type
     */
    protected T handleCharacter(Character character) {
        return handleString(Character.toString(character));
    }

    /**
     * Hook method to handle a function value.  By default, it calls {@link #handle(Object)} on the first argument
     * supplied or {@link #handleNull()} if the arguments are empty.
     *
     * @param function the function
     * @return type
     */
    protected T handleFunction(Function<Object, Object> function) {
        if (arguments.length == 0) {
            return handleNull();
        }

        return handle(function.apply(arguments[0]));
    }

    /**
     * Hook method to handle an int range.  By default, it just delegates to {@link #handleObject(Object)}.
     *
     * @param range the int range
     * @return type
     */
    protected T handleIntRange(CoreIntRange range) {
        return handleObject(range);
    }

    /**
     * Hook method to handle an iterable.  By default, it just delegates to {@link #handleList(List)}}.
     *
     * @param iterable the iterable
     * @return type
     */
    protected T handleIterable(Iterable<Object> iterable) {
        return handleList(CoreLists.of(iterable));
    }

    /**
     * Hook method to handle a lambda value.  By default, it invokes the lambda with the arguments passed to the
     * constructor and call {@link #handle(Object)} on the result.
     *
     * @param lambda the lambda
     * @return type
     */
    protected T handleLambda(CoreLambda lambda) {
        return handle(lambda.invoke(arguments));
    }

    /**
     * Hook method to handle a list.  By default, it just delegates to {@link #handleListWrapper(CoreListWrapper)}.
     *
     * @param list the list
     * @return type
     */
    protected T handleList(List<Object> list) {
        return handleListWrapper(new CoreListWrapper<>(list));
    }

    /**
     * Hook method to handle a list wrapper.  By default, it just delegates to
     * {@link #handleIndexedCollectionWrapper(CoreIndexedIterableWrapper)}.
     *
     * @param wrapper the list wrapper
     * @return type
     */
    protected T handleListWrapper(CoreListWrapper<Object> wrapper) {
        return handleIndexedCollectionWrapper(wrapper);
    }

    /**
     * Hook method to handle an indexed collection wrapper.  By default, it just delegates to
     * {@link #handleObject(Object)}.
     *
     * @param wrapper the indexed collection wrapper
     * @return type
     */
    protected T handleIndexedCollectionWrapper(CoreIndexedIterableWrapper<Object, ?> wrapper) {
        return handleObject(wrapper.getValue());
    }

    /**
     * Hook method to handle a map value.  By default, it just delegates to {@link #handleObject(Object)}.
     *
     * @param map the map
     * @return type
     */
    protected T handleMap(Map<Object, Object> map) {
        return handleObject(map);
    }

    /**
     * Hook method to handle a number value.  By default, it just delegates to {@link #handleObject(Object)}.
     *
     * @param number the number
     * @return type
     */
    protected T handleNumber(Number number) {
        return handleObject(number);
    }

    /**
     * Hook method to handle a predicate value.  By default, it calls {@link #handle(Object)} on the first argument
     * supplied or {@link #handleNull()} if the arguments are empty.
     *
     * @param predicate the predicate
     * @return type
     */
    protected T handlePredicate(Predicate<Object> predicate) {
        if (arguments.length == 0) {
            return handleNull();
        }

        return handle(predicate.test(arguments[0]));
    }

    /**
     * Hook method to handle a property value.  By default, it just delegates to {@link #handleFunction(Function)}.
     *
     * @param property the property
     * @return type
     */
    protected T handleProperty(CoreProperty property) {
        return handleFunction(property);
    }

    /**
     * Hook method to handle a string value.  By default, it just delegates to {@link #handleObject(Object)}.
     *
     * @param string the string
     * @return type
     */
    protected T handleString(String string) {
        return handleObject(string);
    }

    /**
     * Handle the supplied value is some way.
     *
     * @param value the object
     * @return type
     */
    protected abstract T handleObject(Object value);
}

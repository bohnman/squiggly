package com.github.bohnman.squiggly.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.function.DefaultFunctions;
import com.github.bohnman.squiggly.util.array.ArrayWrappers;
import com.github.bohnman.squiggly.util.range.IntRange;
import net.jcip.annotations.ThreadSafe;

import javax.annotation.Nullable;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * Provides various convenience methods.
 */
@ThreadSafe
public class SquigglyUtils {

    private SquigglyUtils() {
    }


    /**
     * Takes an object and converts it to a string.
     *
     * @param mapper the object mapper
     * @param object the object to convert
     * @return json string
     */
    public static String stringify(ObjectMapper mapper, @Nullable  Object object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static String toString(Object o) {
        if (o == null) {
            return null;
        }

        if (o.getClass().isArray()) {
            return ArrayWrappers.create(o).toString();
        }

        return o.toString();
    }

    public static Number toNumber(Object o) {
        return toNumber(o, 0);
    }

    public static Number toNumber(Object o, Number defaultValue) {
        if (o == null) {
            return defaultValue;
        }

        if (o instanceof Number) {
            return (Number) o;
        }

        if (o instanceof String) {
            try {
                return Double.parseDouble((String) o);
            } catch (NumberFormatException e) {
                return defaultValue;
            }
        }

        return defaultValue;
    }

    @SuppressWarnings("unchecked")
    public static boolean toBoolean(Object o) {
        if (o == null) {
            return false;
        }

        if (o instanceof Boolean) {
            return (Boolean) o;
        }

        if (o instanceof Number) {
            return ((Number) o).doubleValue() != 0;
        }

        if (o instanceof String) {
            return !"".equals(o);
        }

        return false;
    }


    @SuppressWarnings("unchecked")
    public static Integer compare(Object o1, Object o2) {
        if (Objects.equals(o1, o2)) {
            return 0;
        }

        if (o1 == null || o2 == null) {
            return null;
        }

        if (o1 instanceof Number && o2 instanceof Number) {
            double d1 = ((Number) o1).doubleValue();
            double d2 = ((Number) o2).doubleValue();
            return Double.compare(d1, d2);
        }

        if (!(o1 instanceof Comparable)) {
            return null;
        }

        if (!(o2 instanceof Comparable)) {
            return null;
        }

        if (!o1.getClass().isAssignableFrom(o2.getClass())) {
            return null;
        }

        Comparable c1 = (Comparable) o1;
        Comparable c2 = (Comparable) o2;

        return c1.compareTo(c2);
    }

    @SuppressWarnings("unchecked")
    public static Function toFunction(Object o) {
        if (o instanceof Function) {
            return (Function) o;
        }

        if (o instanceof Predicate) {
            return ((Predicate) o)::test;
        }

        return (in) -> o;
    }

    @SuppressWarnings("unchecked")
    public static Predicate toPredicate(Object o) {
        if (o instanceof Predicate) {
            return (Predicate) o;
        }

        if (o instanceof Function) {
            return (in) -> toBoolean(((Function) o).apply(in));
        }

        return (in) -> toBoolean(o);
    }

    public static Object getProperty(Object o, Object key) {
        if (o == null || key == null) {
            return null;
        }

        if (o.getClass().isArray() || o instanceof Iterable) {
            if (key instanceof Number) {
                return DefaultFunctions.first(DefaultFunctions.slice(o, ((Number) key).intValue(), 1));
            }

            if (key instanceof IntRange) {
                return DefaultFunctions.slice(o, (IntRange) key);
            }
        }

        String keyString = toString(key);

        if (o instanceof Map) {
            return ((Map) o).get(keyString);
        }


        return getPropertyDescriptors(o.getClass())
                .filter(propertyDescriptor -> propertyDescriptor.getName().equals(keyString))
                .findFirst()
                .orElse(null);

    }


    public static Stream<PropertyDescriptor> getPropertyDescriptors(Class<?> beanClass)  {
        try {
            return Arrays.stream(Introspector.getBeanInfo(beanClass).getPropertyDescriptors())
                    .filter(propertyDescriptor -> propertyDescriptor.getReadMethod() != null)
                    .filter(propertyDescriptor -> !propertyDescriptor.getName().equals("class"));
        } catch (IntrospectionException e) {
            return Stream.empty();
        }
    }

    public static Object invoke(Method method, Object owner, Object... args) {
        try {
            return method.invoke(owner, args);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }
}

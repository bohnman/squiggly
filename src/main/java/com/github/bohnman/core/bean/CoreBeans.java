package com.github.bohnman.core.bean;

import com.github.bohnman.squiggly.core.function.DefaultFunctions;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.range.CoreIntRange;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Stream;

public class CoreBeans {

    private CoreBeans() {
    }


    public static Stream<PropertyDescriptor> getReadablePropertyDescriptors(Class<?> beanClass) {
        try {
            return Arrays.stream(Introspector.getBeanInfo(beanClass).getPropertyDescriptors())
                    .filter(propertyDescriptor -> propertyDescriptor.getReadMethod() != null)
                    .filter(propertyDescriptor -> !propertyDescriptor.getName().equals("class"));
        } catch (IntrospectionException e) {
            return Stream.empty();
        }
    }

    public static Object getProperty(Object o, Object key) {
        if (o == null || key == null) {
            return null;
        }

        if (o.getClass().isArray() || o instanceof Iterable) {
            if (key instanceof Number) {
                return DefaultFunctions.first(DefaultFunctions.slice(o, ((Number) key).intValue(), 1));
            }

            if (key instanceof CoreIntRange) {
                return DefaultFunctions.slice(o, (CoreIntRange) key);
            }
        }

        String keyString = CoreConversions.toString(key);

        if (o instanceof Map) {
            return ((Map) o).get(keyString);
        }


        return getReadablePropertyDescriptors(o.getClass())
                .filter(propertyDescriptor -> propertyDescriptor.getName().equals(keyString))
                .map(propertyDescriptor -> CoreMethods.invoke(propertyDescriptor.getReadMethod(), o))
                .findFirst()
                .orElse(null);

    }

}

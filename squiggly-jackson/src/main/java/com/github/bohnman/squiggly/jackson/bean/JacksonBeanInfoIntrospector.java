package com.github.bohnman.squiggly.jackson.bean;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.bean.BeanInfoIntrospector;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;

import javax.annotation.Nullable;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class JacksonBeanInfoIntrospector extends BeanInfoIntrospector {

    public JacksonBeanInfoIntrospector(SquigglyConfig config, SquigglyMetrics metrics) {
        super(config, metrics);
    }

    @Nullable
    @Override
    protected String getPropertyName(String propertyName, Annotation[] annotations) {
        if (propertyName != null) {
            return propertyName;
        }

        for (Annotation ann : annotations) {
            if (ann instanceof JsonProperty) {
                propertyName = getPropertyName((JsonProperty) ann);

                if (propertyName != null) {
                    return propertyName;
                }

            }

            for (Annotation classAnn : ann.annotationType().getAnnotations()) {
                if (classAnn instanceof JsonProperty) {
                    propertyName = getPropertyName((JsonProperty) classAnn);

                    if (propertyName != null) {
                        return propertyName;
                    }
                }
            }
        }

        return null;
    }

    protected String getPropertyName(JsonProperty ann) {
        return CoreStrings.defaultIfEmpty(ann.value(), null);
    }


    @Override
    protected boolean isUnwrapped(PropertyDescriptor propertyDescriptor, Field field) {
        if (field != null && field.isAnnotationPresent(JsonUnwrapped.class)) {
            return true;
        }

        Method readMethod = propertyDescriptor.getReadMethod();

        if (readMethod != null && readMethod.isAnnotationPresent(JsonUnwrapped.class)) {
            return true;
        }

        Method writeMethod = propertyDescriptor.getWriteMethod();

        if (writeMethod != null && writeMethod.isAnnotationPresent(JsonUnwrapped.class)) {
            return true;
        }

        return false;
    }
}

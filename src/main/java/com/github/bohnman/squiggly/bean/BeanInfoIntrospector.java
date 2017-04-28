package com.github.bohnman.squiggly.bean;

import com.fasterxml.jackson.annotation.JsonUnwrapped;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.metric.source.GuavaCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.view.PropertyView;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.reflect.FieldUtils;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Introspects bean classes, looking for @{@link PropertyView} annotations on fields.
 */
@ThreadSafe
public class BeanInfoIntrospector {

    /**
     * Caches bean class to a map of views to property views.
     */
    private static final LoadingCache<Class, BeanInfo> CACHE;
    private static final GuavaCacheSquigglyMetricsSource METRICS_SOURCE;

    static {
        CACHE = CacheBuilder.from(SquigglyConfig.getPropertyDescriptorCacheSpec())
                .build(new CacheLoader<Class, BeanInfo>() {
                    @Override
                    public BeanInfo load(Class key) throws Exception {
                        return introspectClass(key);
                    }
                });
        METRICS_SOURCE = new GuavaCacheSquigglyMetricsSource("squiggly.property.descriptorCache.", CACHE);
    }


    public BeanInfo introspect(Class beanClass) {
        return CACHE.getUnchecked(beanClass);
    }

    private static BeanInfo introspectClass(Class beanClass) {

        Map<String, Set<String>> viewToFieldNames = Maps.newHashMap();
        Set<String> resolved = Sets.newHashSet();
        Set<String> unwrapped = Sets.newHashSet();

        for (PropertyDescriptor propertyDescriptor : getPropertyDescriptors(beanClass)) {

            if (propertyDescriptor.getReadMethod() == null) {
                continue;
            }

            Field field = FieldUtils.getField(propertyDescriptor.getReadMethod().getDeclaringClass(), propertyDescriptor.getName(), true);

            if (isUnwrapped(propertyDescriptor, field)) {
                unwrapped.add(propertyDescriptor.getName());
            }

            if (field == null) {
                continue;
            }

            Set<String> views = introspectField(field);

            for (String view : views) {
                Set<String> fieldNames = viewToFieldNames.get(view);

                if (fieldNames == null) {
                    fieldNames = Sets.newHashSet();
                    viewToFieldNames.put(view, fieldNames);
                }

                fieldNames.add(field.getName());

            }

        }


        viewToFieldNames = makeUnmodifiable(expand(viewToFieldNames));
        unwrapped = Collections.unmodifiableSet(unwrapped);

        return new BeanInfo(viewToFieldNames, unwrapped);
    }

    private static boolean isUnwrapped(PropertyDescriptor propertyDescriptor, Field field) {
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

    private static Map<String, Set<String>> makeUnmodifiable(Map<String, Set<String>> map) {
        for (String key : map.keySet()) {
            map.put(key, Collections.unmodifiableSet(map.get(key)));
        }

        return Collections.unmodifiableMap(map);
    }

    private static PropertyDescriptor[] getPropertyDescriptors(Class beanClass) {
        try {
            return Introspector.getBeanInfo(beanClass).getPropertyDescriptors();
        } catch (IntrospectionException e) {
            throw new RuntimeException("Unable to introspect " + beanClass.getName(), e);
        }
    }

    // apply the base fields to other views if configured to do so.
    private static Map<String, Set<String>> expand(Map<String, Set<String>> viewToPropNames) {

        if (!SquigglyConfig.isFilterImplicitlyIncludeBaseFields()) {
            return viewToPropNames;
        }

        Set<String> baseProps = viewToPropNames.get(PropertyView.BASE_VIEW);

        if (baseProps == null) {
            baseProps = ImmutableSet.of();
        }

        for (Map.Entry<String, Set<String>> entry : viewToPropNames.entrySet()) {
            String viewName = entry.getKey();
            Set<String> propNames = entry.getValue();

            if (!PropertyView.BASE_VIEW.equals(viewName)) {
                propNames.addAll(baseProps);
            }
        }

        return viewToPropNames;
    }

    // grab all the PropertyView (or derived) annotations and return their view names.
    private static Set<String> introspectField(Field field) {

        Set<String> views = Sets.newHashSet();

        for (Annotation ann : field.getAnnotations()) {
            if (ann instanceof PropertyView) {
                views.addAll(Lists.newArrayList(((PropertyView) ann).value()));
            }

            for (Annotation classAnn : ann.annotationType().getAnnotations()) {
                if (classAnn instanceof PropertyView) {
                    views.addAll(Lists.newArrayList(((PropertyView) classAnn).value()));
                }
            }
        }
        if (views.isEmpty() && SquigglyConfig.isPropertyAddNonAnnotatedFieldsToBaseView()) {
            return Collections.singleton(PropertyView.BASE_VIEW);
        }

        return views;
    }

    public static GuavaCacheSquigglyMetricsSource getMetricsSource() {
        return METRICS_SOURCE;
    }
}

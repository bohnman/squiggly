package com.github.bohnman.squiggly.view;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
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
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Introspects bean classes, looking for @{@link PropertyView} annotations on fields.
 */
@ThreadSafe
public class PropertyViewIntrospector {

    /**
     * Caches bean class to a map of views to property views.
     */
    private static final Cache<Class, Map<String, Set<String>>> CACHE;

    static {
        CACHE = CacheBuilder.from(SquigglyConfig.getPropertyDescriptorCacheSpec()).build();
    }


    /**
     * Get all the property (field) names of a given class and view name.
     *
     * @param beanClass the bean class
     * @param viewName  the view name
     * @return a set of property names
     */
    public Set<String> getPropertyNames(Class beanClass, String viewName) {
        Map<String, Set<String>> viewNameToPropertyNames = CACHE.getIfPresent(beanClass);

        if (viewNameToPropertyNames == null) {
            viewNameToPropertyNames = introspect(beanClass);
            CACHE.put(beanClass, viewNameToPropertyNames);
        }

        Set<String> propertyNames = viewNameToPropertyNames.get(viewName);

        if (propertyNames == null) {
            propertyNames = ImmutableSet.of();
        }

        return propertyNames;
    }

    private Map<String, Set<String>> introspect(Class beanClass) {

        Map<String, Set<String>> viewToFieldNames = Maps.newHashMap();
        Set<String> resolved = Sets.newHashSet();

        for (PropertyDescriptor propertyDescriptor : getPropertyDescriptors(beanClass)) {
            if (propertyDescriptor.getReadMethod() == null) {
                continue;
            }

            Field field = FieldUtils.getField(propertyDescriptor.getReadMethod().getDeclaringClass(), propertyDescriptor.getName(), true);

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


        return expand(viewToFieldNames);
    }

    private PropertyDescriptor[] getPropertyDescriptors(Class beanClass) {
        try {
            return Introspector.getBeanInfo(beanClass).getPropertyDescriptors();
        } catch (IntrospectionException e) {
            throw new RuntimeException("Unable to introspect " + beanClass.getName(), e);
        }
    }

    // apply the base fields to other views if configured to do so.
    private Map<String, Set<String>> expand(Map<String, Set<String>> viewToPropNames) {

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
    private Set<String> introspectField(Field field) {

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
}

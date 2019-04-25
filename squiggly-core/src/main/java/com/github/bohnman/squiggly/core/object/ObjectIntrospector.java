package com.github.bohnman.squiggly.core.object;

import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.lang.CoreFields;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.core.metric.sources.CoreCacheMetricsSource;
import com.github.bohnman.squiggly.core.view.annotations.PropertyView;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.*;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Introspects bean classes, looking for @{@link PropertyView} annotations on fields.
 */
@ThreadSafe
public class ObjectIntrospector {

    /**
     * Caches bean class to a map of views to property views.
     */
    private final CoreCache<Class, ObjectDescriptor> cache;
    private final SquigglyConfig config;

    public ObjectIntrospector(SquigglyConfig config, SquigglyMetrics metrics) {
        this.config = notNull(config);
        cache = CoreCacheBuilder.from(config.getPropertyDescriptorCacheSpec()).build();
        metrics.add(new CoreCacheMetricsSource("squiggly.property.descriptor-cache.", cache));
    }


    public ObjectDescriptor introspect(Class objectClass) {
        return cache.computeIfAbsent(objectClass, this::introspectClass);
    }

    private ObjectDescriptor introspectClass(Class objectClass) {

        Map<String, Set<String>> viewToPropertyNames = new HashMap<>();
        Set<String> resolved = new HashSet<>();
        Set<String> unwrapped = new HashSet<>();

        for (PropertyDescriptor propertyDescriptor : getPropertyDescriptors(objectClass)) {

            if (propertyDescriptor.getReadMethod() == null) {
                continue;
            }

            Field field = CoreFields.getField(propertyDescriptor.getReadMethod().getDeclaringClass(), propertyDescriptor.getName());
            String propertyName = getPropertyName(propertyDescriptor, field);


            if (isUnwrapped(propertyDescriptor, field)) {
                unwrapped.add(propertyName);
            }

            Set<String> views = introspectPropertyViews(propertyDescriptor, field);

            for (String view : views) {
                Set<String> fieldNames = viewToPropertyNames.computeIfAbsent(view, k -> new HashSet<>());
                fieldNames.add(propertyName);
            }
        }


        viewToPropertyNames = makeUnmodifiable(expand(viewToPropertyNames));
        unwrapped = Collections.unmodifiableSet(unwrapped);

        return new ObjectDescriptor(viewToPropertyNames, unwrapped);
    }

    private String getPropertyName(PropertyDescriptor propertyDescriptor, Field field) {
        String propertyName = null;

        if (propertyDescriptor.getReadMethod() != null) {
            //noinspection ConstantConditions
            propertyName = getPropertyName(propertyName, propertyDescriptor.getReadMethod().getAnnotations());
        }

        if (propertyDescriptor.getWriteMethod() != null) {
            propertyName = getPropertyName(propertyName, propertyDescriptor.getWriteMethod().getAnnotations());
        }

        if (field != null) {
            propertyName = getPropertyName(propertyName, field.getAnnotations());
        }

        if (propertyName == null) {
            propertyName = propertyDescriptor.getName();
        }

        return propertyName;
    }

    @Nullable
    protected String getPropertyName(@Nullable String propertyName, @Nonnull Annotation[] annotations) {
        return propertyName;
    }

    protected boolean isUnwrapped(@Nonnull PropertyDescriptor propertyDescriptor, @Nullable Field field) {
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
    private Map<String, Set<String>> expand(Map<String, Set<String>> viewToPropNames) {

        Set<String> baseProps = viewToPropNames.get(PropertyView.BASE_VIEW);

        if (baseProps == null) {
            baseProps = Collections.emptySet();
        }

        if (!config.isFilterImplicitlyIncludeBaseFieldsInView()) {

            // make an exception for full view
            Set<String> fullView = viewToPropNames.get(PropertyView.FULL_VIEW);

            if (fullView != null) {
                fullView.addAll(baseProps);
            }

            return viewToPropNames;
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
    private Set<String> introspectPropertyViews(PropertyDescriptor propertyDescriptor, Field field) {

        Set<String> views = new HashSet<>();

        if (propertyDescriptor.getReadMethod() != null) {
            applyPropertyViews(views, propertyDescriptor.getReadMethod().getAnnotations());
        }

        if (propertyDescriptor.getWriteMethod() != null) {
            applyPropertyViews(views, propertyDescriptor.getWriteMethod().getAnnotations());
        }

        if (field != null) {
            applyPropertyViews(views, field.getAnnotations());
        }

        if (views.isEmpty() && config.isPropertyAddNonAnnotatedFieldsToBaseView()) {
            return Collections.singleton(PropertyView.BASE_VIEW);
        }

        return views;
    }

    private void applyPropertyViews(Set<String> views, Annotation[] annotations) {
        for (Annotation ann : annotations) {
            if (ann instanceof PropertyView) {
                views.addAll(Arrays.asList(((PropertyView) ann).value()));
            }

            for (Annotation classAnn : ann.annotationType().getAnnotations()) {
                if (classAnn instanceof PropertyView) {
                    views.addAll(Arrays.asList(((PropertyView) classAnn).value()));
                }
            }
        }
    }
}

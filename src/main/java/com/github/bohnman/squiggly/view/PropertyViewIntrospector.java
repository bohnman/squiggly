package com.github.bohnman.squiggly.view;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import com.google.common.base.MoreObjects;
import com.google.common.cache.Cache;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.*;

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
        int maxSize = SquigglyConfig.getPropertyDescriptorCacheMaxSize();
        CACHE = SquigglyUtils.cacheBuilderWithMaxSize(maxSize).build();
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

        Set<String> resolved = Sets.newHashSet();


        // grab all the descriptors of a class and convert it to a map of view names to property naems
        return expand(Stream.of(getPropertyDescriptors(beanClass))
                .filter(d -> d.getReadMethod() != null)
                .map(d -> FieldUtils.getField(d.getReadMethod().getDeclaringClass(), d.getName(), true))
                .filter(Objects::nonNull)
                .flatMap(field -> introspectField(field).stream().map(viewName -> Pair.of(viewName, field.getName())))
                .collect(groupingBy(Pair::getLeft, collectingAndThen(Collectors.toList(), pairs -> pairs.stream().map(Pair::getRight).collect(toSet())))));

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

        Set<String> baseProps = MoreObjects.firstNonNull(viewToPropNames.get(PropertyView.BASE_VIEW), Collections.emptySet());

        viewToPropNames.forEach((viewName, propNames) -> {
            if (!PropertyView.BASE_VIEW.equals(viewName)) {
                propNames.addAll(baseProps);
            }
        });

        return viewToPropNames;
    }

    // grab all the PropertyView (or derived) annotations and return their view names.
    private Set<String> introspectField(Field field) {

        Set<String> views = Stream.of(field.getAnnotations())
                .flatMap(ann -> {
                    if (ann instanceof PropertyView) {
                        return Stream.of(((PropertyView) ann).value());
                    }

                    Optional<Annotation> classAnnOptional = Stream.of(ann.annotationType().getAnnotations())
                            .filter(classAnn -> classAnn instanceof PropertyView)
                            .findAny();

                    if (classAnnOptional.isPresent()) {
                        return Stream.of(((PropertyView) classAnnOptional.get()).value());
                    }

                    return Stream.empty();
                })
                .collect(toSet());

        if (views.isEmpty()) {
            String defaultView = PropertyView.BASE_VIEW;
            return Collections.singleton(defaultView);
        }

        return views;
    }
}

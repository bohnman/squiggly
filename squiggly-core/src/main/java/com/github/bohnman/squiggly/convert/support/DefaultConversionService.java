package com.github.bohnman.squiggly.convert.support;

import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.lang.CoreClasses;
import com.github.bohnman.core.lang.Null;
import com.github.bohnman.squiggly.convert.ConverterDescriptor;
import com.github.bohnman.squiggly.environment.SquigglyEnvironment;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;

import javax.annotation.Nullable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.function.Function;

/**
 * Implementation of a conversion service that is backed by a map.
 */
public class DefaultConversionService implements SquigglyConversionService {

    private static final ConverterDescriptor IDENTITY = new ConverterDescriptor(Object.class, Object.class, Function.identity());
    private static final ConverterDescriptor NO_MATCH = new ConverterDescriptor(Object.class, Object.class, Function.identity());

    private final CoreCache<ConverterDescriptor, ConverterDescriptor> cache;
    private final Map<ConverterDescriptor, ConverterDescriptor> registeredConverters;

    /**
     * Constructor.
     *
     * @param config            the config
     * @param converterRegistry the registry
     */
    public DefaultConversionService(SquigglyEnvironment config, SquigglyConverterRegistry converterRegistry) {
        this.cache = CoreCacheBuilder.from(config.getConvertCacheSpec()).build();

        Map<Key, ConverterDescriptor> map = new HashMap<>(descriptors.size());

        for (ConverterDescriptor descriptor : descriptors) {
            map.putIfAbsent(new Key(descriptor.getSource(), descriptor.getTarget(), descriptor.getOrder()), descriptor);
        }

        this.registeredConverters = Collections.unmodifiableMap(map);

    }

    @Override
    public boolean canConvert(Class<?> source, Class<?> target) {
        ConverterDescriptor record = getRecord(source, target);

        if (record == NO_MATCH) {
            return false;
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T convert(Object source, Class<T> target) {
        Class<?> sourceClass;

        if (source == null) {
            sourceClass = Null.class;
        } else {
            sourceClass = source.getClass();
        }

        ConverterDescriptor record = getRecord(sourceClass, target);

        if (record == NO_MATCH && source == null) {
            record = IDENTITY;
        }

        if (record == NO_MATCH) {
            throw new IllegalArgumentException(String.format("Cannot convert [%s] from [%s] to [%s]: No converter found", source, source.getClass(), source.getClass()));
        }

        Function converter = record.getConverter();
        T result = (T) converter.apply(source);

        if (result == null && target.isPrimitive()) {
            throw new IllegalArgumentException("A null value cannot be assigned to a primitive type");
        }

        return result;
    }

    @Nullable
    @Override
    public ConverterDescriptor findRecord(Class<?> source, Class<?> target) {
        ConverterDescriptor record = getRecord(source, target);
        return record == NO_MATCH ? null : record;
    }

    private ConverterDescriptor getRecord(Class<?> sourceType, Class<?> targetType) {
        return cache.computeIfAbsent(new Key(sourceType, targetType), this::load);
    }

    private ConverterDescriptor load(Key key) {
        ConverterDescriptor converter = find(key.source, key.target);

        if (converter == null) {
            converter = (key.target.isAssignableFrom(key.source) ? IDENTITY : null);
        }

        return (converter == null) ? NO_MATCH : converter;
    }

    private ConverterDescriptor find(Class<?> sourceType, Class<?> targetType) {
        ConverterDescriptor converter = getRegisteredConverter(new Key(sourceType, targetType));

        if (converter != null) {
            return converter;
        }

        List<Class<?>> sourceCandidates = getClassHierarchy(sourceType);
        List<Class<?>> targetCandidates = getClassHierarchy(targetType);
        ConverterDescriptor softMatch = null;

        for (Class<?> sourceCandidate : sourceCandidates) {
            for (Class<?> targetCandidate : targetCandidates) {
                if (sourceType == sourceCandidate && targetType == targetCandidate) {
                    continue;
                }

                Key convertiblePair = new Key(sourceCandidate, targetCandidate);
                converter = getRegisteredConverter(convertiblePair);

                if (converter != null) {
                    return converter;
                }

                if (sourceCandidate == targetCandidate && sourceCandidate != Object.class && softMatch == null) {
                    softMatch = IDENTITY;
                }
            }
        }
        return softMatch;
    }

    private ConverterDescriptor getRegisteredConverter(Key convertiblePair) {
        return registeredConverters.get(convertiblePair);
    }

    private List<Class<?>> getClassHierarchy(Class<?> type) {
        List<Class<?>> hierarchy = new ArrayList<>(20);
        Set<Class<?>> visited = new HashSet<>(20);
        addToClassHierarchy(0, CoreClasses.resolvePrimitiveIfNecessary(type), false, hierarchy, visited);
        boolean array = type.isArray();

        int i = 0;
        while (i < hierarchy.size()) {
            Class<?> candidate = hierarchy.get(i);
            candidate = (array ? candidate.getComponentType() : CoreClasses.resolvePrimitiveIfNecessary(candidate));
            Class<?> superclass = candidate.getSuperclass();
            if (superclass != null && superclass != Object.class && superclass != Enum.class) {
                addToClassHierarchy(i + 1, candidate.getSuperclass(), array, hierarchy, visited);
            }
            addInterfacesToClassHierarchy(candidate, array, hierarchy, visited);
            i++;
        }

        if (Enum.class.isAssignableFrom(type)) {
            addToClassHierarchy(hierarchy.size(), Enum.class, array, hierarchy, visited);
            addToClassHierarchy(hierarchy.size(), Enum.class, false, hierarchy, visited);
            addInterfacesToClassHierarchy(Enum.class, array, hierarchy, visited);
        }

        addToClassHierarchy(hierarchy.size(), Object.class, array, hierarchy, visited);
        addToClassHierarchy(hierarchy.size(), Object.class, false, hierarchy, visited);
        return hierarchy;
    }

    private void addInterfacesToClassHierarchy(Class<?> type, boolean asArray,
                                               List<Class<?>> hierarchy, Set<Class<?>> visited) {

        for (Class<?> implementedInterface : type.getInterfaces()) {
            addToClassHierarchy(hierarchy.size(), implementedInterface, asArray, hierarchy, visited);
        }
    }

    private void addToClassHierarchy(int index, Class<?> type, boolean asArray,
                                     List<Class<?>> hierarchy, Set<Class<?>> visited) {

        if (asArray) {
            type = Array.newInstance(type, 0).getClass();
        }
        if (visited.add(type)) {
            hierarchy.add(index, type);
        }
    }

    private static class Key {
        private final Class<?> source;
        private final Class<?> target;
        private final int order;

        public Key(Class<?> source, Class<?> target) {
            this(source, target, 0);
        }

        public Key(Class<?> source, Class<?> target, int order) {
            this.source = source;
            this.target = target;
            this.order = order;
        }

        public static Key from(ConverterDescriptor record) {
            return new Key(record.getSource(), record.getTarget());
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Key key = (Key) o;
            return Objects.equals(source, key.source) &&
                    Objects.equals(target, key.target);
        }

        @Override
        public int hashCode() {
            return Objects.hash(source, target);
        }

    }
}

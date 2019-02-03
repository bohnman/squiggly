package com.github.bohnman.squiggly.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.fasterxml.jackson.databind.type.MapType;
import net.jcip.annotations.ThreadSafe;

import java.util.*;

/**
 * Provides various convenience methods.
 *
 * @author rbohn
 * @author DheerajKN
 */
@ThreadSafe
public class SquigglyUtils {

    private SquigglyUtils() {
    }

    /**
     * Convert an object to a collection of maps.
     *
     * @param mapper               the object mapper
     * @param source               the source object
     * @param targetCollectionType the target collection type
     * @return collection
     */
    public static Collection<Map<String, Object>> collectify(ObjectMapper mapper, Object source, Class<? extends Collection> targetCollectionType) {
        return collectify(mapper, source, targetCollectionType, String.class, Object.class);
    }

    /**
     * Convert an object to a collection.
     *
     * @param mapper               the object mapper
     * @param source               the source object
     * @param targetCollectionType the target collection type
     * @param targetElementType    the target collection element type
     * @return collection
     */
    public static <E> Collection<E> collectify(ObjectMapper mapper, Object source, Class<? extends Collection> targetCollectionType, Class<E> targetElementType) {
        CollectionType collectionType = mapper.getTypeFactory().constructCollectionType(targetCollectionType, targetElementType);
        return objectify(mapper, convertToCollection(source), collectionType);
    }


    /**
     * Convert an object to a collection.
     *
     * @param mapper               the object mapper
     * @param source               the source object
     * @param targetCollectionType the target collection type
     * @param targetElementType    the target collection element type
     * @return collection
     */
    public static <E> Collection<E> collectify(ObjectMapper mapper, Object source, Class<? extends Collection> targetCollectionType, JavaType targetElementType) {
        CollectionType collectionType = mapper.getTypeFactory().constructCollectionType(targetCollectionType, targetElementType);
        return objectify(mapper, convertToCollection(source), collectionType);
    }

    /**
     * Convert an object to a collection of maps.
     *
     * @param mapper               the object mapper
     * @param source               the source object
     * @param targetCollectionType the target collection type
     * @param targetKeyType        the target map key type
     * @param targetValueType      the target map value type
     * @return collection
     */
    public static <K, V> Collection<Map<K, V>> collectify(ObjectMapper mapper, Object source, Class<? extends Collection> targetCollectionType, Class<K> targetKeyType, Class<V> targetValueType) {
        MapType mapType = mapper.getTypeFactory().constructMapType(Map.class, targetKeyType, targetValueType);
        return collectify(mapper, convertToCollection(source), targetCollectionType, mapType);
    }

    private static Object convertToCollection(Object source) {
        if (source == null) {
            return null;
        }

        if (source instanceof Collection) {
            return source;
        }

        return Collections.singleton(source);
    }

    /**
     * Convert an object to a list of maps.
     *
     * @param mapper the object mapper
     * @param source the source object
     * @return list
     */
    public static List<Map<String, Object>> listify(ObjectMapper mapper, Object source) {
        return (List<Map<String, Object>>) collectify(mapper, source, List.class);
    }

    /**
     * Convert an object to a list.
     *
     * @param mapper            the object mapper
     * @param source            the source object
     * @param targetElementType the target list element type
     * @return list
     */
    public static <E> List<E> listify(ObjectMapper mapper, Object source, Class<E> targetElementType) {
        return (List<E>) collectify(mapper, source, List.class, targetElementType);
    }

    /**
     * Convert an object to a list.
     *
     * @param mapper            the object mapper
     * @param source            the source object
     * @param targetElementType the target list element type
     * @return list
     */
    public static <E> List<E> listify(ObjectMapper mapper, Object source, JavaType targetElementType) {
        return (List<E>) collectify(mapper, source, List.class, targetElementType);
    }

    /**
     * Convert an object to a collection of maps.
     *
     * @param mapper          the object mapper
     * @param source          the source object
     * @param targetKeyType   the target map key type
     * @param targetValueType the target map value type
     * @return collection
     */
    public static <K, V> List<Map<K, V>> listify(ObjectMapper mapper, Object source, Class<K> targetKeyType, Class<V> targetValueType) {
        return (List<Map<K, V>>) collectify(mapper, source, List.class, targetKeyType, targetValueType);
    }

    /**
     * Converts an object to an object, with squiggly filters applied.
     *
     * @param mapper the object mapper
     * @param source the source to convert
     * @return target instance
     * @see SquigglyUtils#objectify(ObjectMapper, Object, Class)
     */
    public static Object objectify(ObjectMapper mapper, Object source) {
        return objectify(mapper, source, Object.class);
    }

    /**
     * Converts an object to an instance of the target type.  Unlike {@link ObjectMapper#convertValue(Object, Class)},
     * this method will apply Squiggly filters.  It does so by first converting the source to bytes and then re-reading
     * it.
     *
     * @param mapper     the object mapper
     * @param source     the source to convert
     * @param targetType the target class type
     * @return target instance
     */
    public static <T> T objectify(ObjectMapper mapper, Object source, Class<T> targetType) {
        return objectify(mapper, source, mapper.getTypeFactory().constructType(targetType));
    }


    /**
     * Converts an object to an instance of the target type.
     *
     * @param mapper     the object mapper
     * @param source     the source to convert
     * @param targetType the target class type
     * @return target instance
     * @see SquigglyUtils#objectify(ObjectMapper, Object, Class)
     */
    public static <T> T objectify(ObjectMapper mapper, Object source, JavaType targetType) {
        try {
            return mapper.readValue(mapper.writeValueAsBytes(source), targetType);
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Convert an object to a set of maps.
     *
     * @param mapper the object mapper
     * @param source the source object
     * @return set
     */
    public static Set<Map<String, Object>> setify(ObjectMapper mapper, Object source) {
        return (Set<Map<String, Object>>) collectify(mapper, source, Set.class);
    }

    /**
     * Convert an object to a set.
     *
     * @param mapper            the object mapper
     * @param source            the source object
     * @param targetElementType the target set element type
     * @return set
     */
    public static <E> Set<E> setify(ObjectMapper mapper, Object source, Class<E> targetElementType) {
        return (Set<E>) collectify(mapper, source, Set.class, targetElementType);
    }

    /**
     * Convert an object to a set.
     *
     * @param mapper            the object mapper
     * @param source            the source object
     * @param targetElementType the target set element type
     * @return set
     */
    public static <E> Set<E> setify(ObjectMapper mapper, Object source, JavaType targetElementType) {
        return (Set<E>) collectify(mapper, source, Set.class, targetElementType);
    }

    /**
     * Convert an object to a collection of maps.
     *
     * @param mapper          the object mapper
     * @param source          the source object
     * @param targetKeyType   the target map key type
     * @param targetValueType the target map value type
     * @return collection
     */
    public static <K, V> Set<Map<K, V>> setify(ObjectMapper mapper, Object source, Class<K> targetKeyType, Class<V> targetValueType) {
        return (Set<Map<K, V>>) collectify(mapper, source, Set.class, targetKeyType, targetValueType);
    }

    /**
     * Takes an object and converts it to a string.
     *
     * @param mapper the object mapper
     * @param object the object to convert
     * @return json string
     */
    public static String stringify(ObjectMapper mapper, Object object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException(e);
        }
    }
}

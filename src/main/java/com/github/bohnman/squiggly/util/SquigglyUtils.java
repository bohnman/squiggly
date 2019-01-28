package com.github.bohnman.squiggly.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.github.bohnman.squiggly.Squiggly;
import net.jcip.annotations.ThreadSafe;

import java.util.Arrays;

/**
 * Provides various convenience methods.
 */
@ThreadSafe
public class SquigglyUtils {

    private SquigglyUtils() {
    }

    /**
     * Converts an object to an object, with squiggly filters applied.
     *
     * @param mapper     the object mapper
     * @param source     the source to convert
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

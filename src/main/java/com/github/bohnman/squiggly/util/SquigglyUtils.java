package com.github.bohnman.squiggly.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import net.jcip.annotations.ThreadSafe;

import javax.annotation.Nullable;

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
}

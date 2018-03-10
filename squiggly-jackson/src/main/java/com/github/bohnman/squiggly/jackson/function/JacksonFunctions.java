package com.github.bohnman.squiggly.jackson.function;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.io.UncheckedIOException;

public class JacksonFunctions {

    private JacksonFunctions() {
    }

    public static Object parseJson(String json) {
        try {
            return new ObjectMapper().readValue(json, Object.class);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static String toJson(Object object) {
        try {
            return new ObjectMapper().writeValueAsString(object);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}

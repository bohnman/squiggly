package com.github.bohnman.squiggly.gson.function.functions;

import com.google.gson.Gson;

/**
 * Custom functions that are specific to the Gson
 */
public class GsonFunctions {

    private GsonFunctions() {
    }

    /**
     * Parse json into an object.
     *
     * @param json the json to parse
     * @return java object
     */
    public static Object parseJson(String json) {
        return new Gson().fromJson(json, Object.class);
    }

    /**
     * Convert an object to Json.
     *
     * @param object a java object
     * @return json representation
     */
    public static String toJson(Object object) {
        return new Gson().toJson(object);
    }
}

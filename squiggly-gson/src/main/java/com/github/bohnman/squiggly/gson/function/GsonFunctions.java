package com.github.bohnman.squiggly.gson.function;

import com.google.gson.Gson;

public class GsonFunctions {

    private GsonFunctions() {
    }

    public static Object parseJson(String json) {
        return new Gson().fromJson(json, Object.class);
    }

    public static String toJson(Object object) {
        return new Gson().toJson(object);
    }
}

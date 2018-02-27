package com.github.bohnman.squiggly.util.lang;

import com.github.bohnman.squiggly.util.array.CoreArrayWrappers;

public class CoreLangConversions {

    public static String toString(Object o) {
        if (o == null) {
            return null;
        }

        if (o.getClass().isArray()) {
            return CoreArrayWrappers.create(o).toString();
        }

        return o.toString();
    }
}

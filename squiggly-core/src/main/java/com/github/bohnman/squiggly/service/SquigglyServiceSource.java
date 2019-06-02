package com.github.bohnman.squiggly.service;

import javax.annotation.Nullable;

public interface SquigglyServiceSource {

    @Nullable
    <T> T findServiceByType(Class<T> type);

    @SuppressWarnings("unchecked")
    @Nullable
    default <T> T findServiceByName(String name, Class<T> expectedType) {
        Object service = findServiceByName(name);

        if (service == null) {
            return null;
        }

        if (!expectedType.isAssignableFrom(service.getClass())) {
            return null;
        }

        return (T) service;
    }

    @Nullable
    Object findServiceByName(String name);

    default <T> T getServiceByType(Class<T> type) {
        T service = findServiceByType(type);

        if (service == null) {
            throw new NullPointerException(String.format("No service of type [%s] was found.", type.getName()));
        }

        return service;
    }

    default Object getServiceByName(String name) {
        Object service = findServiceByName(name);

        if (service == null) {
            throw new NullPointerException(String.format("No service named [%s] was found.", name));
        }

        return service;
    }

    default <T> T getServiceByName(String name, Class<T> expectedType) {
        T service = findServiceByName(name, expectedType);

        if (service == null) {
            throw new NullPointerException(String.format("No service of type [%s] and name [%s] was found.", name, expectedType));
        }

        return service;
    }
}

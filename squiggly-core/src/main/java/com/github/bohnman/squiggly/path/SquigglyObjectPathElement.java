package com.github.bohnman.squiggly.path;

import java.util.Objects;

import static java.util.Objects.requireNonNull;

// represent a specific point in the path.
public class SquigglyObjectPathElement {
    private final String name;
    private final Class objectClass;

    private SquigglyObjectPathElement(String name, Class<?> objectClass) {
        this.name = requireNonNull(name);
        this.objectClass = requireNonNull(objectClass);
    }

    public String getName() {
        return name;
    }

    public Class<?> getObjectClass() {
        return objectClass;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SquigglyObjectPathElement that = (SquigglyObjectPathElement) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    public static SquigglyObjectPathElement create(String name, Object object) {
        return create(name, object.getClass());
    }

    public static SquigglyObjectPathElement create(String name, Class<?> objectClass) {
        return new SquigglyObjectPathElement(name, objectClass);
    }
}

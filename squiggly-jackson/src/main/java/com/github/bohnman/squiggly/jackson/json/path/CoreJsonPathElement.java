package com.github.bohnman.squiggly.jackson.json.path;

import java.util.Objects;

// represent a specific point in the path.
public class CoreJsonPathElement {
    private final String name;
    private final Class bean;

    public CoreJsonPathElement(String name, Object bean) {
        this.name = name;
        this.bean = bean.getClass();
    }

    public String getName() {
        return name;
    }

    public Class getBeanClass() {
        return bean;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CoreJsonPathElement that = (CoreJsonPathElement) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

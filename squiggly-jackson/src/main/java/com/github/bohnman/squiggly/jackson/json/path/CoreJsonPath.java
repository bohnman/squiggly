package com.github.bohnman.squiggly.jackson.json.path;

import com.github.bohnman.core.lang.CoreAssert;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/*
    Represents the path structuore in the object graph
 */
public class CoreJsonPath {

    private final List<CoreJsonPathElement> elements;

    public CoreJsonPath(List<CoreJsonPathElement> elements) {
        this.elements = elements;
    }

    public List<CoreJsonPathElement> getElements() {
        return elements;
    }

    public CoreJsonPathElement getFirst() {
        return elements.isEmpty() ? null : elements.get(0);
    }

    public CoreJsonPathElement getLast() {
        return elements.isEmpty() ? null : elements.get(elements.size() - 1);
    }

    public boolean isEmpty() {
        return elements.isEmpty();
    }

    public CoreJsonPath add(CoreJsonPathElement element) {
        CoreAssert.notNull(element);
        List<CoreJsonPathElement> newElements = new ArrayList<>(elements.size() + 1);
        newElements.addAll(elements);
        newElements.add(element);
        return new CoreJsonPath(Collections.unmodifiableList(newElements));
    }

    // we use the last element because that is where the json stream context started
    public Class getBeanClass() {
        CoreJsonPathElement last = getLast();
        return last == null ? null : last.getBeanClass();
    }

    // maps aren't cachable
    public boolean isCachable() {
        Class beanClass = getBeanClass();
        return beanClass != null && !Map.class.isAssignableFrom(beanClass);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CoreJsonPath path = (CoreJsonPath) o;
        Class beanClass = getBeanClass();
        Class oBeanClass = path.getBeanClass();

        if (!elements.equals(path.elements)) return false;
        if (beanClass != null ? !beanClass.equals(oBeanClass) : oBeanClass != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = elements.hashCode();
        Class beanClass = getBeanClass();
        result = 31 * result + (beanClass != null ? beanClass.hashCode() : 0);
        return result;
    }


    public static CoreJsonPath empty() {
        return new CoreJsonPath(Collections.emptyList());
    }
}

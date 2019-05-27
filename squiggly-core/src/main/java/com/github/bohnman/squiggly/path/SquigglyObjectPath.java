package com.github.bohnman.squiggly.path;

import com.github.bohnman.core.lang.CoreAssert;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.Objects.requireNonNull;

/*
    Represents the path structuore in the object graph
 */
// TODO: optimize
public class SquigglyObjectPath {

    private final List<SquigglyObjectPathElement> elements;

    public SquigglyObjectPath(List<SquigglyObjectPathElement> elements) {
        this.elements = requireNonNull(elements);
    }

    public List<SquigglyObjectPathElement> getElements() {
        return elements;
    }

    public SquigglyObjectPathElement getFirst() {
        return elements.isEmpty() ? null : elements.get(0);
    }

    public SquigglyObjectPathElement getLast() {
        return elements.isEmpty() ? null : elements.get(elements.size() - 1);
    }

    public boolean isEmpty() {
        return elements.isEmpty();
    }

    public SquigglyObjectPath append(SquigglyObjectPathElement element) {
        CoreAssert.notNull(element);
        List<SquigglyObjectPathElement> newElements = new ArrayList<>(elements.size() + 1);
        newElements.addAll(elements);
        newElements.add(element);
        return new SquigglyObjectPath(Collections.unmodifiableList(newElements));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SquigglyObjectPath path = (SquigglyObjectPath) o;
        Class objectClass = getLastObjectClass();
        Class oObjectClass = path.getLastObjectClass();

        if (!elements.equals(path.elements)) return false;
        if (!Objects.equals(objectClass, oObjectClass)) return false;

        return true;
    }


    @Override
    public int hashCode() {
        int result = elements.hashCode();
        Class beanClass = getLastObjectClass();
        result = 31 * result + (beanClass != null ? beanClass.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return elements.stream().map(SquigglyObjectPathElement::getName).collect(Collectors.joining("."));
    }

    private Class<?> getLastObjectClass() {
        SquigglyObjectPathElement last = getLast();
        return last == null ? null : last.getObjectClass();
    }


    public static SquigglyObjectPath empty() {
        return new SquigglyObjectPath(Collections.emptyList());
    }
}

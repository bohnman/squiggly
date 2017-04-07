package com.github.bohnman.squiggly.examples.dropwizard.web;

import java.util.List;

public class ListResponse<T> {

    private final List<T> items;

    public ListResponse(List<T> items) {
        this.items = items;
    }

    public List<T> getItems() {
        return items;
    }

    public static <T> ListResponse<T> of(List<T> items) {
        return new ListResponse<>(items);
    }
}

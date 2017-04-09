package com.github.bohnman.squiggly.model;

import java.util.Collections;
import java.util.List;

public class Item {

    private final String id;
    private final String name;
    private final List<Item> items;

    public Item(String id, String name) {
        this(id, name, Collections.<Item>emptyList());
    }

    public Item(String id, String name, Item item) {
        this(id, name, Collections.singletonList(item));
    }

    public Item(String id, String name, List<Item> items) {
        this.id = id;
        this.name = name;
        this.items = items;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public List<Item> getItems() {
        return items;
    }

    public static Item testItem() {
        Item item5 = new Item("ITEM-5", "Binoculars");
        Item item4 = new Item("ITEM-4", "Hoverboard", item5);
        Item item3 = new Item("ITEM-3", "Milkshake", item4);
        Item item2 = new Item("ITEM-2", "Life Preserver", item3);

        return new Item("ITEM-1", "Nike Shoes", item2);
    }
}

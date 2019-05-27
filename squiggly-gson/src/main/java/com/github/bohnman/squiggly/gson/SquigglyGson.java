package com.github.bohnman.squiggly.gson;

import com.github.bohnman.squiggly.extend.SquigglyExtension;
import com.github.bohnman.squiggly.filter.support.StaticFilterProvider;
import com.github.bohnman.squiggly.json.SquigglyJson;
import com.github.bohnman.squiggly.runtime.SquigglyRuntime;
import com.github.bohnman.squiggly.runtime.support.DefaultSquigglyRuntime;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.support.SquigglyFunctions;
import com.github.bohnman.squiggly.gson.function.functions.GsonFunctions;
import com.github.bohnman.squiggly.gson.json.nodes.GsonJsonNode;
import com.google.gson.JsonElement;

import java.util.List;

/**
 * Entry point for apply Squiggly to the Gson library.
 */
public class SquigglyGson extends SquigglyJson {

    public SquigglyGson(SquigglyRuntime runtime) {
        super(runtime);
    }

    /**
     * Apply the filters to a json element.
     *
     * @param element the json element
     * @param filters the filters
     * @return transformed element
     */
    public JsonElement apply(JsonElement element, String... filters) {
        return filter(new GsonJsonNode(element), filters).getRawNode();
    }

    /**
     * Initialize with default parameters.
     *
     * @return squiggly
     */
    public static SquigglyGson init() {
        return init(initializer -> {});
    }

    /**
     * Initialize with a filter.
     *
     * @param filter the filter
     * @return squiggly
     */
    public static SquigglyGson init(String filter) {
        return init(initializer -> initializer.filter(new StaticFilterProvider(filter)));
    }

    /**
     * Initialize with an extensions.
     *
     * @param contextProvider
     * @return squigly
     */
    public static SquigglyGson init(SquigglyExtension extension) {
        return new SquigglyGson(new Builder().build());
    }

    /**
     * Custom builder class.
     */
    public static class Builder extends DefaultSquigglyRuntime.Builder {

        @Override
        protected void applyDefaultFunctions(List<SquigglyFunction<?>> functions) {
            super.applyDefaultFunctions(functions);
            functions.addAll(SquigglyFunctions.create(GsonFunctions.class));
        }

    }

}

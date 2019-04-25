package com.github.bohnman.squiggly.gson;

import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.filter.contextproviders.SimpleFilterContextProvider;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.functions.SquigglyFunctions;
import com.github.bohnman.squiggly.gson.function.functions.GsonFunctions;
import com.github.bohnman.squiggly.gson.json.nodes.GsonJsonNode;
import com.google.gson.JsonElement;

import java.util.List;

/**
 * Entry point for apply Squiggly to the Gson library.
 */
public class Squiggly extends BaseSquiggly {

    private Squiggly(BaseBuilder builder) {
        super(builder);
    }

    /**
     * Apply the filters to a json element.
     *
     * @param element the json element
     * @param filters the filters
     * @return transformed element
     */
    public JsonElement apply(JsonElement element, String... filters) {
        return apply(new GsonJsonNode(element), filters).getRawNode();
    }

    /**
     * Initialize with default parameters.
     *
     * @return squiggly
     */
    public static Squiggly init() {
        return builder().build();
    }

    /**
     * Initialize with a filter.
     *
     * @param filter the filter
     * @return squiggly
     */
    public static Squiggly init(String filter) {
        return builder(filter).build();
    }

    /**
     * Initialize with a context provider.
     *
     * @param contextProvider context provider
     * @return squigly
     */
    public static Squiggly init(SquigglyFilterContextProvider contextProvider) {
        return builder(contextProvider).build();
    }

    /**
     * Create a builder that configures Squiggly.
     *
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Create a builder that configures Squiggly with a static filter.
     *
     * @param filter static filter
     * @return builder
     */
    public static Builder builder(String filter) {
        return builder().filterContext(new SimpleFilterContextProvider(filter));
    }

    /**
     * Create a builder that configures Squiggly with a context provider.
     *
     * @param contextProvider context provider
     * @return builder
     */
    public static Builder builder(SquigglyFilterContextProvider contextProvider) {
        return builder().filterContext(contextProvider);
    }

    /**
     * Custom builder class.
     */
    public static class Builder extends BaseBuilder<Builder, Squiggly> {

        @Override
        protected void applyDefaultFunctions(List<SquigglyFunction<?>> functions) {
            super.applyDefaultFunctions(functions);
            functions.addAll(SquigglyFunctions.create(GsonFunctions.class));
        }

        @Override
        protected Squiggly newInstance() {
            return new Squiggly(this);
        }

    }
}

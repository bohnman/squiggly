package com.github.bohnman.squiggly.gson;

import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.SquigglyFunctions;
import com.github.bohnman.squiggly.gson.function.GsonFunctions;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.github.bohnman.squiggly.gson.json.GsonJsonNode;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Squiggly extends BaseSquiggly {

    public Squiggly(BaseBuilder builder) {
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
    public static Squiggly init(SquigglyContextProvider contextProvider) {
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
        return builder().context(new SimpleSquigglyContextProvider(filter));
    }

    /**
     * Create a builder that configures Squiggly with a context provider.
     *
     * @param contextProvider context provider
     * @return builder
     */
    public static Builder builder(SquigglyContextProvider contextProvider) {
        return builder().context(contextProvider);
    }

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


    public static void main(String[] args) {
        Person person = new Person("Ryan", "Bohn", "rbohn", "bohnman", "doogie");
        Gson gson = new Gson();
        JsonElement element = gson.toJsonTree(person);
        JsonElement transformed = Squiggly.init()
                .apply(element, "nickNames[name.reverse()]");

        System.out.println(gson.toJson(transformed));

    }


    public static class NickName implements Comparable<NickName> {
        private final String name;

        public NickName(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }

        public String getChuck() {
            return "chuck";
        }

        @Override
        public int compareTo(@Nullable NickName o) {
            return (o == null) ? -1 : name.compareTo(o.name);
        }
    }

    private static class Person {
        private final String firstName;
        private final String lastName;
        private List<NickName> nickNames;

        public Person(String firstName, String lastName, String... nickNames) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.nickNames = Arrays.stream(nickNames).map(NickName::new).collect(Collectors.toList());
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public List<NickName> getNickNames() {
            return nickNames;
        }

        public String getNullProperty() {
            return null;
        }
    }
}

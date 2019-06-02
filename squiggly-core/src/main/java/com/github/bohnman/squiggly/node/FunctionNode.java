package com.github.bohnman.squiggly.node;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Represents a function call.
 */
public class FunctionNode extends BaseNode {

    private final String name;
    private final List<ArgumentNode> arguments;
    private final boolean ignoreNulls;
    private final Type functionType;
    private final boolean ascending;
    private final boolean initial;

    /**
     * Constuctor.
     *
     * @param origin     parse origin
     * @param name        name of the function
     * @param arguments   function arguments
     * @param ignoreNulls ignore function if input is null
     * @param functionType        function type
     * @param ascending   is sorted ascending
     * @param initial     initial
     */
    private FunctionNode(
            SquigglyNodeOrigin origin,
            String name,
            List<ArgumentNode> arguments,
            boolean ignoreNulls,
            Type functionType,
            boolean ascending,
            boolean initial) {
        super(origin);
        this.name = notNull(name);
        this.arguments = Collections.unmodifiableList(notNull(arguments));
        this.ignoreNulls = ignoreNulls;
        this.functionType = notNull(functionType);
        this.ascending = ascending;
        this.initial = initial;
    }

    /**
     * Get the function name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the function arguments
     *
     * @return arguments
     */
    public List<ArgumentNode> getArguments() {
        return arguments;
    }

    /**
     * Get whether to ignore the function when the input is null.
     *
     * @return ignore
     */
    public boolean isIgnoreNulls() {
        return ignoreNulls;
    }

    /**
     * Get the type of the function.
     *
     * @return type
     */
    public Type getFunctionType() {
        return functionType;
    }

    /**
     * Get whether to sort ascending.  This is mainly used by properties.
     *
     * @return ascending
     */
    public boolean isAscending() {
        return ascending;
    }

    public boolean isInitial() {
        return initial;
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", name, arguments);
    }

    /**
     * Assists with construction of the function node.
     */
    public static class Builder extends BaseNodeBuilder<FunctionNode> {

        @Nullable
        private String name;

        private List<ArgumentNode> arguments = new ArrayList<>();
        private boolean ignoreNulls;

        private Type type = Type.FUNCTION;
        private boolean ascending = true;
        private boolean initial;

        Builder(SquigglyNodeOrigin origin) {
            super(origin);
        }


        /**
         * Sets the function name
         *
         * @param name name
         * @return builder
         */
        public Builder name(String name) {
            this.name = name;
            return this;
        }

        /**
         * Adds a function argument.
         *
         * @param arg argument
         * @return builder
         */
        public Builder argument(ArgumentNode.Builder arg) {
            int index = arguments.size();
            arguments.add(arg.index(index).build());
            return this;
        }

        /**
         * Sets whether to ignore null inputs.
         *
         * @param ignore ignore nulls
         * @return builder
         */
        public Builder ignoreNulls(boolean ignore) {
            this.ignoreNulls = ignore;
            return this;
        }

        /**
         * Sets the function type.
         *
         * @param type function type
         * @return builder
         */
        public Builder type(Type type) {
            this.type = type;
            return this;
        }

        /**
         * Sets the sort order.
         *
         * @param ascending ascending
         * @return builder
         */
        public Builder ascending(boolean ascending) {
            this.ascending = ascending;
            return this;
        }

        /**
         * Set the initial.
         *
         * @param initial initial
         * @return builder
         */
        public Builder initial(boolean initial) {
            this.initial = initial;
            return this;
        }

        /**
         * Build the node.
         *
         * @return node
         */
        public FunctionNode build() {
            return new FunctionNode(getOrigin(), name, arguments, ignoreNulls, type, ascending, initial);
        }
    }

    /**
     * Indicates the category of the function node.
     */
    public enum Type {

        /**
         * A function that performs an assignment.  Eg. foo=bar
         */
        ASSIGNMENT,

        /**
         * A regular function.
         */
        FUNCTION,

        /**
         * A property retrieving function.
         */
        PROPERTY,
    }
}

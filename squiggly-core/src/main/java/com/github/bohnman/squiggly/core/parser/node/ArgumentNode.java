package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.parser.ParseContext;

import javax.annotation.Nullable;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Represents a function arguments.
 */
public class ArgumentNode {
    private final ParseContext context;
    private final int index;
    private final Object value;
    private final ArgumentNodeType type;


    /**
     * Constructor.
     *
     * @param context parse context
     * @param index argument index
     * @param value argument value
     * @param type argument type
     */
    public ArgumentNode(ParseContext context, int index, Object value, ArgumentNodeType type) {
        this.context = notNull(context);
        isTrue(index >= 0, "index must be >= 0");
        this.index = index;

        this.value = value;
        this.type = notNull(type);
    }

    /**
     * Parse context.
     *
     * @return parse context
     */
    public ParseContext getContext() {
        return context;
    }

    /**
     * Argument index.
     *
     * @return arg index
     */
    public int getIndex() {
        return index;
    }

    /**
     * Argument value.
     *
     * @return value
     */
    public Object getValue() {
        return value;
    }

    /**
     * Argument type.
     *
     * @return type
     */
    public ArgumentNodeType getType() {
        return type;
    }

    @Override
    public String toString() {
        return "{" +
                "value=" + value +
                ", type=" + type +
                '}';
    }

    /**
     * Create a new builder for easy construction.
     *
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for easy construction.
     */
    public static class Builder {
        @Nullable
        private ParseContext context;
        private int index = -1;

        @Nullable
        private Object value;

        @Nullable
        private ArgumentNodeType type;

        private Builder() {
        }

        /**
         * Set the parse context
         *
         * @param context parse context
         * @return builder
         */
        public Builder context(ParseContext context) {
            this.context = context;
            return this;
        }

        /**
         * Set the index
         *
         * @param index index
         * @return builder
         */
        public Builder index(int index) {
            this.index = index;
            return this;
        }

        /**
         * Set the value
         *
         * @param value value
         * @return builder
         */
        public Builder value(Object value) {
            this.value = value;
            return this;
        }

        /**
         * Set the type
         *
         * @param type tu[e
         * @return builder
         */
        public Builder type(ArgumentNodeType type) {
            this.type = type;
            return this;
        }

        /**
         * Build the node
         *
         * @return node
         */
        public ArgumentNode build() {
            return new ArgumentNode(context, index, value, type);
        }
    }
}

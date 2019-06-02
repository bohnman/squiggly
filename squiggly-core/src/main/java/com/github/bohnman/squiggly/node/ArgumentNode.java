package com.github.bohnman.squiggly.node;

import javax.annotation.Nullable;

import static com.github.bohnman.core.lang.CoreAssert.isTrue;
import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Represents a function arguments.
 */
public class ArgumentNode extends BaseNode {
    private final int index;
    private final Object value;
    private final Type argumentType;


    /**
     * Constructor.
     *
     * @param origin parse origin
     * @param index   argument index
     * @param value   argument value
     * @param argumentType    argument type
     */
    private ArgumentNode(SquigglyNodeOrigin origin, int index, Object value, Type argumentType) {
        super(origin);
        isTrue(index >= 0, "index must be >= 0");
        this.index = index;

        this.value = value;
        this.argumentType = notNull(argumentType);
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
    public Type getArgumentType() {
        return argumentType;
    }

    @Override
    public String toString() {
        return "{" +
                "value=" + value +
                ", type=" + argumentType +
                '}';
    }

    /**
     * Builder for easy construction.
     */
    public static class Builder extends BaseNodeBuilder<ArgumentNode> {
        private int index = -1;

        @Nullable
        private Object value;

        @Nullable
        private Type type;

        Builder(SquigglyNodeOrigin origin) {
            super(origin);
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
        public Builder value(@Nullable Object value) {
            this.value = value;
            return this;
        }

        /**
         * Set the type
         *
         * @param type tu[e
         * @return builder
         */
        public Builder type(Type type) {
            this.type = type;
            return this;
        }

        /**
         * Build the node
         *
         * @return node
         */
        public ArgumentNode build() {
            return new ArgumentNode(getOrigin(), index, value, type);
        }
    }

    /**
     * Indicates what the an argument represents.
     */
    public enum Type {
        ARRAY_DECLARATION,
        ARRAY_RANGE_DECLARATION,
        BOOLEAN,
        FLOAT,
        FUNCTION_CHAIN,
        IF,
        INPUT,
        INT_RANGE,
        INTEGER,
        LAMBDA,
        OBJECT_DECLARATION,
        NULL,
        REGEX,
        STRING,
        VARIABLE
    }
}

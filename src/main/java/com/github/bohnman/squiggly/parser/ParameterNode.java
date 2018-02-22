package com.github.bohnman.squiggly.parser;

import javax.annotation.Nullable;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

public class ParameterNode {
    private final ParseContext context;
    private final int index;
    private final Object value;
    private final ParameterNodeType type;


    public ParameterNode(ParseContext context, int index, Object value, ParameterNodeType type) {
        this.context = checkNotNull(context);
        checkArgument(index >= 0, "index must be >= 0");
        this.index = index;

        this.value = checkNotNull(value);
        this.type = checkNotNull(type);
    }

    public ParseContext getContext() {
        return context;
    }

    public int getIndex() {
        return index;
    }

    public Object getValue() {
        return value;
    }

    public ParameterNodeType getType() {
        return type;
    }

    @Override
    public String toString() {
        return "{" +
                "value=" + value +
                ", type=" + type +
                '}';
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        @Nullable
        private ParseContext context;
        private int index = -1;

        @Nullable
        private Object value;

        @Nullable
        private ParameterNodeType type;

        private Builder() {
        }

        public Builder context(ParseContext context) {
            this.context = context;
            return this;
        }

        public Builder index(int index) {
            this.index = index;
            return this;
        }

        public Builder value(Object value) {
            this.value = value;
            return this;
        }

        public Builder type(ParameterNodeType type) {
            this.type = type;
            return this;
        }

        public ParameterNode builder() {
            return new ParameterNode(context, index, value, type);
        }
    }
}

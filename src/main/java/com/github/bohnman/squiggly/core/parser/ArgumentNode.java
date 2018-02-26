package com.github.bohnman.squiggly.core.parser;

import javax.annotation.Nullable;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

public class ArgumentNode {
    private final ParseContext context;
    private final int index;
    private final Object value;
    private final ArgumentNodeType type;


    public ArgumentNode(ParseContext context, int index, Object value, ArgumentNodeType type) {
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
        private ArgumentNodeType type;

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

        public Builder type(ArgumentNodeType type) {
            this.type = type;
            return this;
        }

        public ArgumentNode build() {
            return new ArgumentNode(context, index, value, type);
        }
    }
}

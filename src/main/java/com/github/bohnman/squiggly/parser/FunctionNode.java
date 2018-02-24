package com.github.bohnman.squiggly.parser;

import com.google.common.collect.ImmutableList;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

public class FunctionNode {

    private final ParseContext context;
    private final String name;
    private final List<ArgumentNode> parameters;
    private final boolean ignoreNulls;

    public FunctionNode(ParseContext context, String name, List<ArgumentNode> parameters, boolean ignoreNulls) {
        this.context = checkNotNull(context);
        this.name = checkNotNull(name);
        this.parameters = ImmutableList.copyOf(checkNotNull(parameters));
        this.ignoreNulls = ignoreNulls;
    }

    public ParseContext getContext() {
        return context;
    }

    public String getName() {
        return name;
    }

    public List<ArgumentNode> getParameters() {
        return parameters;
    }

    public boolean isIgnoreNulls() {
        return ignoreNulls;
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", name, parameters);
    }

    public static class Builder {

        @Nullable
        private ParseContext context;

        @Nullable
        private String name;

        private List<ArgumentNode> parameters = new ArrayList<>();
        private boolean ignoreNulls;

        private Builder() {
        }

        public Builder context(ParseContext context) {
            this.context = context;
            return this;
        }

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder parameter(ArgumentNode.Builder arg) {
            int index = parameters.size();
            parameters.add(arg.index(index).build());
            return this;
        }

        public Builder ignoreNulls(boolean ignore) {
            this.ignoreNulls = ignore;
            return this;
        }

        public FunctionNode build() {
            return new FunctionNode(context, name, parameters, ignoreNulls);
        }
    }
}

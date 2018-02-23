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

    public FunctionNode(ParseContext context, String name, List<ArgumentNode> parameters) {
        this.context = checkNotNull(context);
        this.name = checkNotNull(name);
        this.parameters = ImmutableList.copyOf(checkNotNull(parameters));
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

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {

        @Nullable
        private ParseContext context;

        @Nullable
        private String name;

        private List<ArgumentNode> parameters = new ArrayList<>();

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

        public FunctionNode build() {
            return new FunctionNode(context, name, parameters);
        }
    }
}

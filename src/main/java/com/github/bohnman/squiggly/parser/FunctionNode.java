package com.github.bohnman.squiggly.parser;

import com.google.common.collect.ImmutableList;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

public class FunctionNode {

    private final ParseContext context;
    private final String name;
    private final List<ParameterNode> parameters;

    public FunctionNode(ParseContext context, String name, List<ParameterNode> parameters) {
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

    public List<ParameterNode> getParameters() {
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

        private List<ParameterNode> parameters = new ArrayList<>();

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

        public Builder parameter(ParseContext context, Object value, ParameterType type) {
            int index = parameters.size();
            ParameterNode parameter = ParameterNode.builder()
                    .context(context)
                    .index(index)
                    .value(value)
                    .type(type)
                    .builder();
            parameters.add(parameter);
            return this;
        }

        public FunctionNode build() {
            return new FunctionNode(context, name, parameters);
        }
    }
}

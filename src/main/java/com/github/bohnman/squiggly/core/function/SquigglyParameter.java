package com.github.bohnman.squiggly.core.function;

import javax.annotation.Nullable;

import static com.google.common.base.Preconditions.checkNotNull;

public class SquigglyParameter {

    private final Class<?> type;
    private final boolean varArgs;

    public SquigglyParameter(Class<?> type, boolean varArgs) {
        this.type = checkNotNull(type);
        this.varArgs = varArgs;
    }

    public Class<?> getType() {
        return type;
    }

    public boolean isVarArgs() {
        return varArgs;
    }

    public static Builder builder(Class<?> type) {
        return new Builder().type(type);
    }

    public static class Builder {
        private Builder() {
        }

        @Nullable
        private Class<?> type;

        private boolean varArgs;

        public Builder type(Class<?> type) {
            this.type = type;
            return this;
        }

        public Builder varArgs(boolean varArgs) {
            this.varArgs = varArgs;
            return this;
        }

        public SquigglyParameter build() {
            return new SquigglyParameter(type, varArgs);
        }
    }
}

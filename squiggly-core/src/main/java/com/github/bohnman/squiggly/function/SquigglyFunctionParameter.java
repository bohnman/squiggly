package com.github.bohnman.squiggly.function;

import javax.annotation.Nullable;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Represents a function parameter.
 */
public class SquigglyFunctionParameter {

    private final Class<?> type;
    private final boolean varArgs;

    /**
     * Constructor.
     *
     * @param type    parameter type
     * @param varArgs is the parameter a var args type
     */
    public SquigglyFunctionParameter(Class<?> type, boolean varArgs) {
        this.type = notNull(type);
        this.varArgs = varArgs;
    }

    /**
     * Parameter type.
     *
     * @return type
     */
    public Class<?> getType() {
        return type;
    }

    /**
     * Parameter varargs indicator.
     *
     * @return varargs
     */
    public boolean isVarArgs() {
        return varArgs;
    }

    /**
     * Provide a builder for constructing a parameter.
     *
     * @param type paramerter type
     * @return builder
     */
    public static Builder builder(Class<?> type) {
        return new Builder().type(type);
    }

    /**
     * Builder for constructing a parameter.
     */
    public static class Builder {
        private Builder() {
        }

        @Nullable
        private Class<?> type;

        private boolean varArgs;

        /**
         * Set the parameter type.
         *
         * @param type type
         * @return builder
         */
        public Builder type(Class<?> type) {
            this.type = type;
            return this;
        }

        /**
         * Set whether or not the parameter is varargs.
         *
         * @param varArgs variable args
         * @return builder
         */
        public Builder varArgs(boolean varArgs) {
            this.varArgs = varArgs;
            return this;
        }

        /**
         * Builder the parameter.
         *
         * @return parameter
         */
        public SquigglyFunctionParameter build() {
            return new SquigglyFunctionParameter(type, varArgs);
        }
    }
}

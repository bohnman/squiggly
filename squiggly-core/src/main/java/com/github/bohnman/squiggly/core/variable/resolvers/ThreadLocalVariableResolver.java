package com.github.bohnman.squiggly.core.variable.resolvers;

import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariablesHolder;

import javax.annotation.Nullable;
import java.util.Map;

public class ThreadLocalVariableResolver implements SquigglyVariableResolver {

    @Nullable
    @Override
    public Object resolveVariable(String name) {
        Map<String, Object> variables = SquigglyVariablesHolder.get();

        if (variables == null) {
            return null;
        }

        return variables.get(name);
    }
}

package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

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

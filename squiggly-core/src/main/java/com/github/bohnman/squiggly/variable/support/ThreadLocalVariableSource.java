package com.github.bohnman.squiggly.variable.support;

import com.github.bohnman.squiggly.variable.SquigglyVariableSource;

import javax.annotation.Nullable;
import java.util.Map;

public class ThreadLocalVariableSource implements SquigglyVariableSource {

    @Nullable
    @Override
    public Object findVariableByName(String name) {
        Map<String, Object> variables = SquigglyVariablesHolder.get();

        if (variables == null) {
            return null;
        }

        return variables.get(name);
    }
}

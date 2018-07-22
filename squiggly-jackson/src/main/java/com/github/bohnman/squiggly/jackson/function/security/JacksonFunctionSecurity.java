package com.github.bohnman.squiggly.jackson.function.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.introspect.BeanPropertyDefinition;
import com.fasterxml.jackson.databind.type.SimpleType;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.function.security.SquigglyFunctionSecurity;

import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

public class JacksonFunctionSecurity implements SquigglyFunctionSecurity {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public boolean isPropertyViewable(Object key, Class pojoClass) {
        if (key == null) {
            return true;
        }

        if (pojoClass.isPrimitive()) {
            return true;
        }

        if (pojoClass.isArray()) {
            return true;
        }

        if (Iterable.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Boolean.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Character.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Function.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Predicate.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (CoreIntRange.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Map.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (Number.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        if (String.class.isAssignableFrom(pojoClass)) {
            return true;
        }

        return objectMapper.getSerializationConfig().introspect(SimpleType.construct(pojoClass))
                .findProperties()
                .stream()
                .map(BeanPropertyDefinition::getName)
                .anyMatch(name -> name.equals(key));
    }
}

package com.github.bohnman.squiggly.variable;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.Person;
import com.github.bohnman.squiggly.Squiggly;

import javax.annotation.Nullable;
import java.util.Arrays;

import static com.google.common.base.Preconditions.checkNotNull;

public class CompositeVariableResolver implements SquigglyVariableResolver {

    private final Iterable<SquigglyVariableResolver> resolvers;

    public CompositeVariableResolver(SquigglyVariableResolver... resolvers) {
        this(Arrays.asList(resolvers));
    }

    public CompositeVariableResolver(Iterable<SquigglyVariableResolver> resolvers) {
        this.resolvers = checkNotNull(resolvers);
    }

    @Nullable
    @Override
    public Object resolveVariable(String name, @Nullable Object defaultValue) {
        Object value = null;


        for (SquigglyVariableResolver resolver : resolvers) {
            value = resolver.resolveVariable(name);

            if (value != null) {
                break;
            }
        }

        if (value == null) {
            value = defaultValue;
        }

        return value;
    }

    public static void main(String[] args) throws JsonProcessingException {
        System.out.println("A");
        ObjectMapper mapper = Squiggly.builder(":foo")
                .variable("foo", "lastName")
                .build()
                .apply(new ObjectMapper());
        System.out.println("B");
        System.out.println(mapper.writeValueAsString(new Person("Ryan", "Bohn")));
        System.out.println("C");
    }
}

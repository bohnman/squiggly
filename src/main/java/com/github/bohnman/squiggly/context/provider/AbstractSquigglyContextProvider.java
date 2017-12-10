package com.github.bohnman.squiggly.context.provider;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.github.bohnman.squiggly.context.LazySquigglyContext;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.parser.SquigglyParser;

/**
 * Base implemention of a provider that implements base functionality.
 */
public abstract class AbstractSquigglyContextProvider implements SquigglyContextProvider {

    private final SquigglyParser parser;

    public AbstractSquigglyContextProvider() {
        this(new SquigglyParser());
    }

    public AbstractSquigglyContextProvider(SquigglyParser parser) {
        this.parser = parser;
    }

    @Override
    public SquigglyContext getContext(Class beanClass) {
        return new LazySquigglyContext(beanClass, parser, getFilter(beanClass));
    }

    @Override
    public boolean isFilteringEnabled() {
        return true;
    }

    /**
     * Get the filter expression.
     *
     * @param beanClass class of the top-level bean being filtered
     * @return filter expression
     */
    protected abstract String getFilter(Class beanClass);


    @Override
    public void serializeAsIncludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception {
        writer.serializeAsField(pojo, jgen, provider);
    }

    @Override
    public void serializeAsExcludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception {
        writer.serializeAsOmittedField(pojo, jgen, provider);
    }
}

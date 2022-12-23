package dev.nicklasw.squiggly.context.provider;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import dev.nicklasw.squiggly.context.SquigglyContext;

/**
 * Used for supplying a @{@link dev.nicklasw.squiggly.filter.SquigglyPropertyFilter} with a way to retrieve a
 * context.
 */
public interface SquigglyContextProvider {

    /**
     * Get the context.
     *
     * @param beanClass the class of the top-level bean being filtered
     * @return context
     */
    SquigglyContext getContext(Class beanClass);

    /**
     * Hook method to enable/disable filtering.
     *
     * @return ture if enabled, false if not
     */
    boolean isFilteringEnabled();

    // Hook method for custom included serialization
    void serializeAsIncludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception;

    // Hook method for custom excluded serialization
    void serializeAsExcludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception;
}

package com.github.bohnman.squiggly.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.PropertyWriter;

public interface SquigglySerializer {

    // Hook method for custom included serialization
    default void serializeAsIncludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception {
        writer.serializeAsField(pojo, jgen, provider);
    }

    // Hook method for custom excluded serialization
    default void serializeAsExcludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception {
        writer.serializeAsOmittedField(pojo, jgen, provider);
    }

    default void serializeAsConvertedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer, Object value) throws Exception {
        jgen.writeFieldName(writer.getName());
        jgen.writeObject(value);
    }
}

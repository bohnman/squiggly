package com.github.bohnman.squiggly.jackson;

import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.support.StaticFilterProvider;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.function.support.SquigglyFunctions;
import com.github.bohnman.squiggly.jackson.filter.SquigglyPropertyFilter;
import com.github.bohnman.squiggly.jackson.filter.SquigglyPropertyFilterMixin;
import com.github.bohnman.squiggly.jackson.function.functions.JacksonFunctions;
import com.github.bohnman.squiggly.jackson.function.security.JacksonFunctionSecurity;
import com.github.bohnman.squiggly.jackson.json.nodes.JacksonJsonNode;
import com.github.bohnman.squiggly.jackson.object.introspectors.JacksonObjectIntrospector;
import com.github.bohnman.squiggly.jackson.serialize.SquigglyJacksonSerializer;
import com.github.bohnman.squiggly.json.SquigglyJson;
import com.github.bohnman.squiggly.json.filter.SquigglyJsonNodeFilter;
import com.github.bohnman.squiggly.runtime.support.DefaultSquigglyRuntime;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.io.File;
import java.io.IOException;
import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Provides various way of registering a {@link SquigglyPropertyFilter} with a Jackson ObjectMapper.
 */
@ThreadSafe
public class SquigglyJackson extends SquigglyJson {

    private final SquigglyPropertyFilter filter;
    private final SquigglyJacksonSerializer serializer;
    private volatile boolean propertyFilterApplied;

    private SquigglyJackson(Builder builder) {
        super(builder, new JacksonObjectIntrospector(builder.getBuiltConfig(), builder.getBuiltMetrics()));
        this.filter = new SquigglyPropertyFilter(this);
        this.serializer = notNull(builder.builtSerializer);
    }

    @Override
    protected SquigglyJsonNodeFilter createNodeFilter() {
        return new SquigglyJsonNodeFilter(this) {
            @Override
            protected boolean appendContextFilter() {
                Boolean append = getConfig().getAppendContextInNodeFilter();

                if (append == null && propertyFilterApplied) {
                    return false;
                }

                return CoreObjects.firstNonNull(append, true);
            }
        };
    }

    public JsonNode apply(ObjectMapper mapper, Object object, String... filters) {
        JsonNode node = object instanceof JsonNode ? (JsonNode) object : mapper.valueToTree(object);
        return filter(new JacksonJsonNode(node), filters).getRawNode();
    }

    /**
     * Apply squiggly to the provided object mapper.
     *
     * @param mapper object mapper
     * @return object mapper
     */
    @SuppressWarnings("deprecation")
    public ObjectMapper apply(ObjectMapper mapper) {
        propertyFilterApplied = true;
        FilterProvider filterProvider = mapper.getSerializationConfig().getFilterProvider();
        SimpleFilterProvider simpleFilterProvider;

        if (filterProvider instanceof SimpleFilterProvider) {
            simpleFilterProvider = (SimpleFilterProvider) filterProvider;
        } else if (filterProvider == null) {
            simpleFilterProvider = new SimpleFilterProvider();
            mapper.setFilters(simpleFilterProvider);
        } else {
            throw new IllegalStateException("Unable to register squiggly filter with FilterProvider of type " + filterProvider.getClass().getName() + ".  You'll have to register the filter manually");

        }

        simpleFilterProvider.addFilter(SquigglyPropertyFilter.FILTER_ID, filter);
        mapper.addMixIn(Object.class, SquigglyPropertyFilterMixin.class);

        return mapper;
    }

    /**
     * Apply squiggly to all of the provided object mappers
     *
     * @param mappers the object mappers
     */
    public void applyAll(Iterable<ObjectMapper> mappers) {
        for (ObjectMapper mapper : mappers) {
            apply(mapper);
        }
    }

    /**
     * Apply squiggly to all of the provided object mappers
     *
     * @param mappers the object mappers
     */
    public void applyAll(ObjectMapper... mappers) {
        for (ObjectMapper mapper : mappers) {
            apply(mapper);
        }
    }

    /**
     * Gets filter.
     *
     * @return filter
     */
    public SquigglyPropertyFilter getFilter() {
        return filter;
    }

    /**
     * Get the serializer.
     *
     * @return serialzer
     */
    public SquigglyJacksonSerializer getSerializer() {
        return serializer;
    }

    /**
     * Create a builder that configures Squiggly.
     *
     * @return builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Create a builder that configures Squiggly with a static filter.
     *
     * @param filter static filter
     * @return builder
     */
    public static Builder builder(String filter) {
        return builder().filterContextProvider(new StaticFilterProvider(filter));
    }

    /**
     * Create a builder that configures Squiggly with a context provider.
     *
     * @param contextProvider context provider
     * @return builder
     */
    public static Builder builder(SquigglyFilterContextProvider contextProvider) {
        return builder().filterContextProvider(contextProvider);
    }


    public static SquigglyJackson init() {
        return builder().build();
    }

    public static SquigglyJackson init(String filter) {
        return builder(filter).build();
    }

    public static SquigglyJackson init(SquigglyFilterContextProvider provider) {
        return builder(provider).build();
    }


    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a static filter expression.
     *
     * @param mapper the Jackson Object Mapper
     * @param filter the filter expressions
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static ObjectMapper init(ObjectMapper mapper, String filter) throws IllegalStateException {
        return init(mapper, new StaticFilterProvider(filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a static filter expression.
     *
     * @param mappers the Jackson Object Mappers to init
     * @param filter  the filter expressions
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, String filter) throws IllegalStateException {
        init(mappers, new StaticFilterProvider(filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mapper          the Jackson Object Mapper
     * @param contextProvider the context provider to use
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static ObjectMapper init(ObjectMapper mapper, SquigglyFilterContextProvider contextProvider) throws IllegalStateException {
        return builder(contextProvider).build().apply(mapper);
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mappers         the Jackson Object Mappers to init
     * @param contextProvider the context provider to use
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, SquigglyFilterContextProvider contextProvider) {
        builder(contextProvider).build().applyAll(mappers);
    }


    /**
     * Helper class that configures squiggly
     */
    public static class Builder extends DefaultSquigglyRuntime.Builder<Builder, SquigglyJackson> {

        @Nullable
        private SquigglyJacksonSerializer serializer;

        @Nullable
        private SquigglyJacksonSerializer builtSerializer;

        private Builder() {
        }


        /**
         * Sets a serializer.
         *
         * @param serializer serializer
         * @return builder
         */
        public Builder serializer(SquigglyJacksonSerializer serializer) {
            this.serializer = serializer;
            return this;
        }

        @Override
        protected void applyDefaultFunctions(List<SquigglyFunction<?>> functions) {
            super.applyDefaultFunctions(functions);
            functions.addAll(SquigglyFunctions.create(JacksonFunctions.class));
        }

        @Override
        protected SquigglyJackson newInstance() {
            this.builtSerializer = buildSerializer();

            return new SquigglyJackson(this);
        }

        private SquigglyJacksonSerializer buildSerializer() {
            SquigglyJacksonSerializer serializer = this.serializer;

            if (serializer == null) {
                serializer = new SquigglyJacksonSerializer() {
                };
            }
            return serializer;
        }

        @Override
        protected SquigglyFunctionSecurity buildFunctionSecurity() {
            if (functionSecurity != null) {
                return functionSecurity;
            }

            return new JacksonFunctionSecurity();
        }
    }


    public static void main(String[] args) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        String include = "companyList.companySubGroupNames.companySubGrpGivenNames,companyList.directorsDetails,companyList.type,";
        String exclude = "-companyList.companySubGroupNames.companySubGrpGivenNames.createdDate,-companyList.directorsDetails.qualifications";
        String filter;

        filter = "companyList[companySubGroupNames[companySubGrpGivenNames[-createdDate]],directorsDetails[-qualifications],type]";

        SquigglyJackson.init(mapper, filter);
        Object value = mapper.readValue(new File("/Users/rbohn/Downloads/test.txt"), Object.class);
        mapper.writer(new DefaultPrettyPrinter()).writeValue(System.out, value);
    }
}

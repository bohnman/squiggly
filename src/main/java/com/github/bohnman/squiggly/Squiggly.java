package com.github.bohnman.squiggly;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.github.bohnman.squiggly.bean.BeanInfoIntrospector;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.config.source.SquigglyConfigSource;
import com.github.bohnman.squiggly.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilter;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilterMixin;
import com.github.bohnman.squiggly.filter.repository.CompositeFilterRepository;
import com.github.bohnman.squiggly.filter.repository.MapFilterRepository;
import com.github.bohnman.squiggly.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.parser.SquigglyParser;
import com.github.bohnman.squiggly.serializer.SquigglySerializer;
import com.github.bohnman.squiggly.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.variable.MapVariableResolver;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;
import net.jcip.annotations.ThreadSafe;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Provides various way of registering a {@link SquigglyPropertyFilter} with a Jackson ObjectMapper.
 */
@ThreadSafe
public class Squiggly {

    private final BeanInfoIntrospector beanInfoIntrospector;
    private final SquigglyPropertyFilter filter;
    private final SquigglyConfig config;
    private final SquigglyMetrics metrics;
    private final SquigglyContextProvider contextProvider;
    private final SquigglyFilterRepository filterRepository;
    private final SquigglyParser parser;
    private final SquigglySerializer serializer;
    private final SquigglyVariableResolver variableResolver;

    public Squiggly(
            SquigglyConfig config,
            SquigglyContextProvider contextProvider,
            SquigglyFilterRepository filterRepository,
            SquigglyMetrics metrics,
            SquigglyParser parser,
            SquigglySerializer serializer,
            SquigglyVariableResolver variableResolver) {
        this.beanInfoIntrospector = new BeanInfoIntrospector(config, metrics);
        this.config = checkNotNull(config);
        this.contextProvider = checkNotNull(contextProvider);
        this.filterRepository = checkNotNull(filterRepository);
        this.metrics = checkNotNull(metrics);
        this.parser = checkNotNull(parser);
        this.serializer = checkNotNull(serializer);
        this.variableResolver = checkNotNull(variableResolver);
        this.filter = new SquigglyPropertyFilter(this);
    }

    /**
     * Apply squiggly to the provided object mapper.
     *
     * @param mapper object mapper
     * @return object mapper
     */
    @SuppressWarnings("deprecation")
    public ObjectMapper apply(ObjectMapper mapper) {
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
     * Gets bean info introspector.
     *
     * @return introspector
     */
    public BeanInfoIntrospector getBeanInfoIntrospector() {
        return beanInfoIntrospector;
    }

    /**
     * Get the configuration information.
     *
     * @return config
     */
    public SquigglyConfig getConfig() {
        return config;
    }

    /**
     * Get context provider.
     *
     * @return context provider
     */
    public SquigglyContextProvider getContextProvider() {
        return contextProvider;
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
     * Get filter repo.
     *
     * @return filter repo
     */
    public SquigglyFilterRepository getFilterRepository() {
        return filterRepository;
    }

    /**
     * Get the metrics.
     *
     * @return metrics
     */
    public SquigglyMetrics getMetrics() {
        return metrics;
    }

    /**
     * Get the parser.
     *
     * @return parser
     */
    public SquigglyParser getParser() {
        return parser;
    }

    /**
     * Get the serializer.
     *
     * @return serialzer
     */
    public SquigglySerializer getSerializer() {
        return serializer;
    }

    /**
     * Get the variable resolver.
     *
     * @return variable resolver
     */
    public SquigglyVariableResolver getVariableResolver() {
        return variableResolver;
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
        return builder().staticFilter(filter);
    }

    /**
     * Create a builder that configures Squiggly with a context provider.
     *
     * @param contextProvider context provider
     * @return builder
     */
    public static Builder builder(SquigglyContextProvider contextProvider) {
        return builder().context(contextProvider);
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
        return init(mapper, new SimpleSquigglyContextProvider(filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a static filter expression.
     *
     * @param mappers the Jackson Object Mappers to init
     * @param filter  the filter expressions
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, String filter) throws IllegalStateException {
        init(mappers, new SimpleSquigglyContextProvider(filter));
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mapper          the Jackson Object Mapper
     * @param contextProvider the context provider to use
     * @return object mapper, mainly for convenience
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static ObjectMapper init(ObjectMapper mapper, SquigglyContextProvider contextProvider) throws IllegalStateException {
        return builder(contextProvider).build().apply(mapper);
    }

    /**
     * Initialize a @{@link SquigglyPropertyFilter} with a specific context provider.
     *
     * @param mappers         the Jackson Object Mappers to init
     * @param contextProvider the context provider to use
     * @throws IllegalStateException if the filter was unable to be registered
     */
    public static void init(Iterable<ObjectMapper> mappers, SquigglyContextProvider contextProvider) {
        builder(contextProvider).build().applyAll(mappers);
    }


    /**
     * Helper class that configures squiggly
     *
     * @param <B> the builder type
     * @param <S> the squiggly type
     */
    public static class Builder<B extends Builder<B, S>, S extends Squiggly> {

        private final List<SquigglyConfigSource> configSources = new ArrayList<>();

        @Nullable
        private SquigglyContextProvider contextProvider;

        @Nullable
        private SquigglyFilterRepository filterRepository;

        private final Map<String, String> savedFilters = new HashMap<>();

        @Nullable
        private SquigglySerializer serializer;

        @Nullable
        private SquigglyVariableResolver variableResolver;

        private final Map<String, Object> variables = new HashMap<>();

        private Builder() {
        }

        /**
         * Add a config source.
         *
         * @param source the config source
         * @return builder
         */
        public B config(SquigglyConfigSource source) {
            configSources.add(checkNotNull(source));
            return getThis();
        }

        /**
         * Set a context provider.
         *
         * @param contextProvider the context provider
         * @return builder
         */
        public B context(SquigglyContextProvider contextProvider) {
            this.contextProvider = contextProvider;
            return getThis();
        }

        /**
         * Sets the filter repository.
         *
         * @param filterRepository filter repository
         * @return builder
         */
        public B filterRepository(SquigglyFilterRepository filterRepository) {
            this.filterRepository = filterRepository;
            return getThis();
        }

        /**
         * Adds a saved filter.
         *
         * @param name   name
         * @param filter filter
         * @return builder
         */
        public B savedFilter(String name, String filter) {
            savedFilters.put(name, filter);
            return getThis();
        }

        /**
         * Sets a serializer.
         *
         * @param serializer serializer
         * @return builder
         */
        public B serializer(SquigglySerializer serializer) {
            this.serializer = serializer;
            return getThis();
        }

        /**
         * Set a static filter.
         *
         * @param filter the filter
         * @return builder
         */
        public B staticFilter(String filter) {
            return context(new SimpleSquigglyContextProvider(filter));
        }

        /**
         * Sets the variable resolver.
         *
         * @param variableResolver variable resolver
         * @return builder
         */
        public B variableResolver(SquigglyVariableResolver variableResolver) {
            this.variableResolver = variableResolver;
            return getThis();
        }

        /**
         * Sets a global variable.
         *
         * @param name  name
         * @param value value
         * @return builder
         */
        public B variable(String name, Object value) {
            variables.put(name, value);
            return getThis();
        }

        /**
         * Build the squiggly object.
         *
         * @return squiggly object
         */
        @SuppressWarnings("unchecked")
        public S build() {
            SquigglyContextProvider contextProvider = this.contextProvider;
            SquigglyConfig config = new SquigglyConfig(configSources);
            SquigglyMetrics metrics = new SquigglyMetrics();
            SquigglyParser parser = new SquigglyParser(config, metrics);
            SquigglySerializer serializer = this.serializer;

            if (contextProvider == null) {
                contextProvider = new SimpleSquigglyContextProvider();
            }


            if (serializer == null) {
                serializer = new SquigglySerializer() {
                };
            }

            SquigglyFilterRepository filterRepository = new MapFilterRepository(savedFilters);

            if (this.filterRepository != null) {
                filterRepository = new CompositeFilterRepository(this.filterRepository, filterRepository);
            }

            SquigglyVariableResolver variableResolver = new MapVariableResolver(variables);

            if (this.variableResolver != null) {
                variableResolver = new CompositeVariableResolver(this.variableResolver, variableResolver);
            }

            return (S) new Squiggly(config, contextProvider, filterRepository, metrics, parser, serializer, variableResolver);
        }

        @SuppressWarnings("unchecked")
        private B getThis() {
            return (B) this;
        }
    }

}

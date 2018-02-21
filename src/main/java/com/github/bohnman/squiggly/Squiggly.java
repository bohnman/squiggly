package com.github.bohnman.squiggly;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.github.bohnman.squiggly.bean.BeanInfoIntrospector;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.config.source.SquigglyConfigSource;
import com.github.bohnman.squiggly.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.convert.DefaultConversionService;
import com.github.bohnman.squiggly.convert.DefaultConverters;
import com.github.bohnman.squiggly.convert.NullSafeConverterRecord;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilter;
import com.github.bohnman.squiggly.filter.SquigglyPropertyFilterMixin;
import com.github.bohnman.squiggly.filter.repository.CompositeFilterRepository;
import com.github.bohnman.squiggly.filter.repository.MapFilterRepository;
import com.github.bohnman.squiggly.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.function.DefaultFunctions;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyFunctions;
import com.github.bohnman.squiggly.function.repository.CompositeFunctionRepository;
import com.github.bohnman.squiggly.function.repository.MapFunctionRepository;
import com.github.bohnman.squiggly.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.parser.SquigglyParser;
import com.github.bohnman.squiggly.serializer.SquigglySerializer;
import com.github.bohnman.squiggly.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.variable.MapVariableResolver;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;
import com.google.common.collect.Streams;
import net.jcip.annotations.ThreadSafe;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.google.common.base.Preconditions.checkNotNull;
import static java.util.stream.Collectors.toList;

/**
 * Provides various way of registering a {@link SquigglyPropertyFilter} with a Jackson ObjectMapper.
 */
@ThreadSafe
public class Squiggly {

    private final BeanInfoIntrospector beanInfoIntrospector;
    private final SquigglyPropertyFilter filter;
    private final SquigglyConfig config;
    private final SquigglyConversionService conversionService;
    private final SquigglyMetrics metrics;
    private final SquigglyContextProvider contextProvider;
    private final SquigglyFilterRepository filterRepository;
    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyParser parser;
    private final SquigglySerializer serializer;
    private final SquigglyVariableResolver variableResolver;

    public Squiggly(
            SquigglyConfig config,
            SquigglyContextProvider contextProvider,
            SquigglyConversionService conversionService,
            SquigglyFilterRepository filterRepository,
            SquigglyFunctionRepository functionRepository, SquigglyMetrics metrics,
            SquigglyParser parser,
            SquigglySerializer serializer,
            SquigglyVariableResolver variableResolver) {
        this.beanInfoIntrospector = new BeanInfoIntrospector(config, metrics);
        this.config = checkNotNull(config);
        this.conversionService = checkNotNull(conversionService);
        this.contextProvider = checkNotNull(contextProvider);
        this.filterRepository = checkNotNull(filterRepository);
        this.functionRepository = checkNotNull(functionRepository);
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
     * Get the conversion service.
     *
     * @return conversion service
     */
    public SquigglyConversionService getConversionService() {
        return conversionService;
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
     * Get function repository.
     *
     * @return repo
     */
    public SquigglyFunctionRepository getFunctionRepository() {
        return functionRepository;
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
    @SuppressWarnings("TypeParameterHidesVisibleType")
    public static class Builder<B extends Builder<B, S>, S extends Squiggly> {

        private final List<SquigglyConfigSource> configSources = new ArrayList<>();

        @Nullable
        private SquigglyContextProvider contextProvider;

        @Nullable
        private Function<List<ConverterRecord>, SquigglyConversionService> conversionService;

        @Nullable
        private SquigglyFilterRepository filterRepository;

        @Nullable
        private SquigglyFunctionRepository functionRepository;

        private final List<SquigglyFunction<?>> functions = new ArrayList<>();

        private boolean registerDefaultConverters = true;

        private boolean registerDefaultFunctions = true;

        private final Map<String, String> savedFilters = new HashMap<>();

        @Nullable
        private SquigglySerializer serializer;

        @Nullable
        private SquigglyVariableResolver variableResolver;

        private final Map<String, Object> variables = new HashMap<>();

        private List<ConverterRecord> converterRecords = new ArrayList<>();

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
         * Supply a conversion service using a factory method.
         *
         * @param factory factory method
         * @return builder
         */
        public B conversionService(Function<List<ConverterRecord>, SquigglyConversionService> factory) {
            this.conversionService = factory;
            return getThis();
        }

        /**
         * Adds a converter.
         *
         * @param source    source class
         * @param target    target class
         * @param converter converter
         * @param <S>       source type
         * @param <T>       target type
         * @return builder
         */
        public <S, T> B converter(Class<S> source, Class<T> target, Function<S, T> converter) {
            this.converterRecords.add(new ConverterRecord(source, target, converter));
            return getThis();
        }

        /**
         * Adds a null safe converter.
         *
         * @param source    source class
         * @param target    target class
         * @param converter converter
         * @param <S>       source type
         * @param <T>       target type
         * @return builder
         */
        public <S, T> B nullSafeConverter(Class<S> source, Class<T> target, Function<S, T> converter) {
            this.converterRecords.add(new NullSafeConverterRecord(source, target, converter));
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
         * Sets the function repository.
         *
         * @param functionRepository function repository
         * @return builder
         */
        public B functionRepository(SquigglyFunctionRepository functionRepository) {
            this.functionRepository = functionRepository;
            return getThis();
        }

        /**
         * Register a function.
         *
         * @param function a function
         * @return builder
         */
        public B function(SquigglyFunction<?> function) {
            this.functions.add(checkNotNull(function));
            return getThis();
        }

        /**
         * Register multiple functions.
         *
         * @param functions functions
         * @return builder
         */
        public B functions(SquigglyFunction<?>... functions) {
            Arrays.stream(functions).forEach(this::function);
            return getThis();
        }

        /**
         * Register multiple functions.
         *
         * @param functions functions
         * @return builder
         */
        public B functions(Iterable<SquigglyFunction<?>> functions) {
            Streams.stream(functions).forEach(this::function);
            return getThis();
        }

        /**
         * Indicate whether or not to register default functions.
         *
         * @param registerDefaultConverters flag
         * @return true/false
         */
        public B registerDefaultConverters(boolean registerDefaultConverters) {
            this.registerDefaultConverters = registerDefaultConverters;
            return getThis();
        }

        /**
         * Indicate whether or not to register default functions.
         *
         * @param registerDefaultFunctions flag
         * @return true/false
         */
        public B registerDefaultFunctions(boolean registerDefaultFunctions) {
            this.registerDefaultFunctions = registerDefaultFunctions;
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
            SquigglyConfig config = new SquigglyConfig(configSources);
            SquigglyContextProvider contextProvider = buildContextProvider();
            SquigglyConversionService conversionService = buildConversionService(config);
            SquigglyFilterRepository filterRepository = buildFilterRepository();
            SquigglyFunctionRepository functionRepository = buildFunctionRepository();
            SquigglyMetrics metrics = new SquigglyMetrics();
            SquigglyParser parser = new SquigglyParser(config, metrics);
            SquigglySerializer serializer = buildSerializer();
            SquigglyVariableResolver variableResolver = buildVariableResolver();


            return (S) new Squiggly(
                    config,
                    contextProvider,
                    conversionService,
                    filterRepository,
                    functionRepository,
                    metrics,
                    parser,
                    serializer,
                    variableResolver);
        }

        private SquigglySerializer buildSerializer() {
            SquigglySerializer serializer = this.serializer;

            if (serializer == null) {
                serializer = new SquigglySerializer() {
                };
            }
            return serializer;
        }

        private SquigglyContextProvider buildContextProvider() {
            SquigglyContextProvider contextProvider = this.contextProvider;

            if (contextProvider == null) {
                contextProvider = new SimpleSquigglyContextProvider();
            }
            return contextProvider;
        }

        private SquigglyConversionService buildConversionService(SquigglyConfig config) {
            List<ConverterRecord> defaultConverterRecords = registerDefaultConverters ? DefaultConverters.get() : Collections.emptyList();
            List<ConverterRecord> allConverterRecords = Stream.concat(defaultConverterRecords.stream(), converterRecords.stream())
                    .collect(toList());

            if (this.conversionService == null) {
                return new DefaultConversionService(config, allConverterRecords);
            }

            return this.conversionService.apply(allConverterRecords);
        }

        private SquigglyVariableResolver buildVariableResolver() {
            SquigglyVariableResolver variableResolver = new MapVariableResolver(variables);

            if (this.variableResolver != null) {
                variableResolver = new CompositeVariableResolver(this.variableResolver, variableResolver);
            }
            return variableResolver;
        }

        private SquigglyFilterRepository buildFilterRepository() {
            SquigglyFilterRepository filterRepository = new MapFilterRepository(savedFilters);

            if (this.filterRepository != null) {
                filterRepository = new CompositeFilterRepository(this.filterRepository, filterRepository);
            }
            return filterRepository;
        }

        private SquigglyFunctionRepository buildFunctionRepository() {
            List<SquigglyFunction<?>> defaultFunctions = getDefaultFunctions();
            List<SquigglyFunctionRepository> functionRepositories = new ArrayList<>(3);

            if (!functions.isEmpty()) {
                functionRepositories.add(new MapFunctionRepository(functions));
            }

            if (this.functionRepository != null) {
                functionRepositories.add(this.functionRepository);
            }

            if (!defaultFunctions.isEmpty()) {
                functionRepositories.add(new MapFunctionRepository(defaultFunctions));
            }

            if (functionRepositories.isEmpty()) {
                return new MapFunctionRepository();
            }

            if (functionRepositories.size() == 1) {
                return functionRepositories.get(0);
            }

            return new CompositeFunctionRepository(functionRepositories);
        }

        @SuppressWarnings("unchecked")
        private List<SquigglyFunction<?>> getDefaultFunctions() {
            if (!registerDefaultFunctions) {
                return Collections.emptyList();
            }

            return (List) SquigglyFunctions.create(DefaultFunctions.class, SquigglyFunction.RegistrationStrategy.MANUAL);
        }

        @SuppressWarnings("unchecked")
        private B getThis() {
            return (B) this;
        }
    }

}

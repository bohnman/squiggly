package com.github.bohnman.squiggly.core;

import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.config.source.SquigglyConfigSource;
import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.convert.ConverterRecord;
import com.github.bohnman.squiggly.core.convert.ConverterRecordRepository;
import com.github.bohnman.squiggly.core.convert.DefaultConversionService;
import com.github.bohnman.squiggly.core.convert.DefaultConverters;
import com.github.bohnman.squiggly.core.convert.ListRecordRepository;
import com.github.bohnman.squiggly.core.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.core.filter.repository.CompositeFilterRepository;
import com.github.bohnman.squiggly.core.filter.repository.MapFilterRepository;
import com.github.bohnman.squiggly.core.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.core.function.DefaultFunctions;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.SquigglyFunctions;
import com.github.bohnman.squiggly.core.function.SystemFunctions;
import com.github.bohnman.squiggly.core.function.repository.CompositeFunctionRepository;
import com.github.bohnman.squiggly.core.function.repository.MapFunctionRepository;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.core.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.util.stream.Collectors.toList;

public abstract class BaseSquiggly {


    private final SquigglyConfig config;
    private final SquigglyConversionService conversionService;
    private final SquigglyMetrics metrics;
    private final SquigglyContextProvider contextProvider;
    private final SquigglyFilterRepository filterRepository;
    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyParser parser;
    private final SquigglyVariableResolver variableResolver;

    protected BaseSquiggly(BaseSquiggly.BaseBuilder builder) {
        this.config = notNull(builder.builtConfig);
        this.conversionService = notNull(builder.builtConversionService);
        this.contextProvider = notNull(builder.builtContextProvider);
        this.filterRepository = notNull(builder.builtFilterRepository);
        this.functionRepository = notNull(builder.builtFunctionRepository);
        this.metrics = notNull(builder.builtMetrics);
        this.parser = notNull(builder.builtParser);
        this.variableResolver = notNull(builder.builtVariableResolver);
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
     * Get the variable resolver.
     *
     * @return variable resolver
     */
    public SquigglyVariableResolver getVariableResolver() {
        return variableResolver;
    }

    /**
     * Helper class that configures squiggly
     *
     * @param <B> the builder type
     * @param <S> the squiggly type
     */
    @SuppressWarnings("TypeParameterHidesVisibleType")
    public abstract static class BaseBuilder<B extends BaseBuilder<B, S>, S extends BaseSquiggly> {

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
        private SquigglyVariableResolver variableResolver;

        private final Map<String, Object> variables = new HashMap<>();

        private List<ConverterRecord> converterRecords = new ArrayList<>();

        // Built Properties
        @Nullable
        protected SquigglyConfig builtConfig;

        @Nullable
        protected SquigglyContextProvider builtContextProvider;

        @Nullable
        protected SquigglyConversionService builtConversionService;

        @Nullable
        protected SquigglyFilterRepository builtFilterRepository;

        @Nullable
        protected SquigglyFunctionRepository builtFunctionRepository;

        @Nullable
        protected SquigglyMetrics builtMetrics;

        @Nullable
        protected SquigglyParser builtParser;

        @Nullable
        protected SquigglyVariableResolver builtVariableResolver;

        protected BaseBuilder() {
        }

        /**
         * Add a config source.
         *
         * @param source the config source
         * @return builder
         */
        public B config(SquigglyConfigSource source) {
            configSources.add(notNull(source));
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
            this.functions.add(notNull(function));
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
            CoreStreams.of(functions).forEach(this::function);
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
            this.builtConfig = new SquigglyConfig(configSources);
            this.builtContextProvider = buildContextProvider();
            this.builtConversionService = buildConversionService(builtConfig);
            this.builtFilterRepository = buildFilterRepository();
            this.builtFunctionRepository = buildFunctionRepository();
            this.builtMetrics = new SquigglyMetrics();
            this.builtParser = new SquigglyParser(builtConfig, builtMetrics);
            this.builtVariableResolver = buildVariableResolver();


            return newInstance();
        }

        @Nullable
        public SquigglyConfig getBuiltConfig() {
            return builtConfig;
        }

        @Nullable
        public SquigglyContextProvider getBuiltContextProvider() {
            return builtContextProvider;
        }

        @Nullable
        public SquigglyConversionService getBuiltConversionService() {
            return builtConversionService;
        }

        @Nullable
        public SquigglyFilterRepository getBuiltFilterRepository() {
            return builtFilterRepository;
        }

        @Nullable
        public SquigglyFunctionRepository getBuiltFunctionRepository() {
            return builtFunctionRepository;
        }

        @Nullable
        public SquigglyMetrics getBuiltMetrics() {
            return builtMetrics;
        }

        @Nullable
        public SquigglyParser getBuiltParser() {
            return builtParser;
        }

        @Nullable
        public SquigglyVariableResolver getBuiltVariableResolver() {
            return builtVariableResolver;
        }

        protected abstract S newInstance();


        private SquigglyContextProvider buildContextProvider() {
            SquigglyContextProvider contextProvider = this.contextProvider;

            if (contextProvider == null) {
                contextProvider = new SimpleSquigglyContextProvider();
            }
            return contextProvider;
        }

        private SquigglyConversionService buildConversionService(SquigglyConfig config) {
            ConverterRecordRepository recordRepository = new ListRecordRepository();

            if (registerDefaultConverters) {
                DefaultConverters.add(recordRepository);
            }

            recordRepository.addAll(converterRecords);

            if (this.conversionService == null) {
                return new DefaultConversionService(config, recordRepository.findAll());
            }

            return this.conversionService.apply(recordRepository.findAll());
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
            List<SquigglyFunction<Object>> coreFunctions = SquigglyFunctions.create(SystemFunctions.class, SquigglyFunction.RegistrationStrategy.MANUAL);


            if (!registerDefaultFunctions) {
                return (List) coreFunctions;
            }

            List<SquigglyFunction<Object>> defaultFunctions = SquigglyFunctions.create(DefaultFunctions.class, SquigglyFunction.RegistrationStrategy.MANUAL);

            return Stream.concat(coreFunctions.stream(), defaultFunctions.stream())
                    .collect(toList());
        }

        @SuppressWarnings("unchecked")
        private B getThis() {
            return (B) this;
        }
    }

}

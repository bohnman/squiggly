package com.github.bohnman.squiggly.engine.support;

import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.convert.support.PrimaryConversionService;
import com.github.bohnman.squiggly.engine.SquigglyEngine;
import com.github.bohnman.squiggly.engine.SquigglyEngineInitializer;
import com.github.bohnman.squiggly.filter.support.SimpleFilterContextProvider;
import com.github.bohnman.squiggly.function.*;
import com.github.bohnman.squiggly.introspect.ObjectIntrospector;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.convert.*;
import com.github.bohnman.squiggly.convert.support.SystemConverters;
import com.github.bohnman.squiggly.convert.support.ListConverterRegistry;
import com.github.bohnman.squiggly.filter.SquigglyNodeFilter;
import com.github.bohnman.squiggly.filter.support.CompositeFilterRepository;
import com.github.bohnman.squiggly.filter.support.MapFilterRepository;
import com.github.bohnman.squiggly.filter.SquigglyFilterRepository;
import com.github.bohnman.squiggly.function.support.SquigglyFunctions;
import com.github.bohnman.squiggly.function.support.DefaultFunctions;
import com.github.bohnman.squiggly.function.support.CompositeFunctionRepository;
import com.github.bohnman.squiggly.function.support.MapFunctionRepository;
import com.github.bohnman.squiggly.filter.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.metric.support.SquigglyMetrics;
import com.github.bohnman.squiggly.parse.SquigglyParser;
import com.github.bohnman.squiggly.variable.support.CompositeVariableResolver;
import com.github.bohnman.squiggly.variable.support.MapVariableResolver;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public abstract class BaseSquigglyEngine implements SquigglyEngine {


    private final ObjectIntrospector objectIntrospector;
    private final SquigglyConfig config;
    private final SquigglyConversionService conversionService;
    private final SquigglyFilterContextProvider contextProvider;
    private final SquigglyFilterRepository filterRepository;
    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyFunctionSecurity functionSecurity;
    private final SquigglyMetrics metrics;
    private final SquigglyExpressionMatcher nodeMatcher;
    private final SquigglyNodeFilter nodeFilter;
    private final SquigglyParser parser;
    private final SquigglyVariableResolver variableResolver;
    private final Function<Object, Object> serviceLocator;

    protected BaseSquigglyEngine(Builder builder) {
        this(builder, new ObjectIntrospector(builder.getBuiltConfig(), builder.getBuiltMetrics()));
    }

    @SuppressWarnings("unchecked")
    protected BaseSquigglyEngine(Builder builder, ObjectIntrospector objectIntrospector) {
        this.objectIntrospector = notNull(objectIntrospector);
        this.config = notNull(builder.builtConfig);
        this.conversionService = notNull(builder.builtConversionService);
        this.contextProvider = notNull(builder.builtContextProvider);
        this.filterRepository = notNull(builder.builtFilterRepository);
        this.functionRepository = notNull(builder.builtFunctionRepository);
        this.functionSecurity = notNull(builder.builtFunctionSecurity);
        this.metrics = notNull(builder.builtMetrics);
        this.parser = notNull(builder.builtParser);
        this.variableResolver = notNull(builder.builtVariableResolver);
        this.functionInvoker = new SquigglyFunctionInvoker(this);
        this.nodeMatcher = new SquigglyExpressionMatcher(this);
        this.serviceLocator = notNull(builder.builtServiceLocator);
        this.nodeFilter = createNodeFilter();
    }

    protected SquigglyNodeFilter createNodeFilter() {
        return new SquigglyNodeFilter(this);
    }


    @Override
    public <T> CoreJsonNode<T> filter(CoreJsonNode<T> node, String... filters) {
        return nodeFilter.apply(node, filters);
    }

    /**
     * Find an object by a class or null if not found.
     *
     * @param type class of the type expected
     * @param <T>  return type expected
     * @return object or null
     */
    @Override
    @Nullable
    public <T> T find(Class<T> type) {
        return find(type, type);
    }

    /**
     * Find an object by a key or null if not found.
     *
     * @param key  a key
     * @param type class of the type expected
     * @param <T>  return type expected
     * @return object or null
     */
    @Override
    @SuppressWarnings("unchecked")
    @Nullable
    public <T> T find(Object key, Class<T> type) {
        return (T) serviceLocator.apply(key);
    }

    /**
     * Find an object by a class or throw an exception if null.
     *
     * @param type class of the type expected
     * @param <T>  return type expected
     * @return object or null
     */
    @Override
    public <T> T get(Class<T> type) {
        return get(type, type);
    }

    /**
     * Find an object by a key or throw an exception if null.
     *
     * @param key  a key
     * @param type class of the type expected
     * @param <T>  return type expected
     * @return object or null
     */
    @Override
    public <T> T get(Object key, Class<T> type) {
        return Objects.requireNonNull(find(key, type));
    }

    /**
     * Gets the bean info introspector
     *
     * @return bean info introspector
     */
    @Override
    public ObjectIntrospector getObjectIntrospector() {
        return objectIntrospector;
    }

    /**
     * Get the configuration information.
     *
     * @return config
     */
    @Override
    public SquigglyConfig getConfig() {
        return config;
    }

    /**
     * Get the conversion service.
     *
     * @return conversion service
     */
    @Override
    public SquigglyConversionService getConversionService() {
        return conversionService;
    }

    /**
     * Get context provider.
     *
     * @return context provider
     */
    @Override
    public SquigglyFilterContextProvider getContextProvider() {
        return contextProvider;
    }

    /**
     * Get filter repo.
     *
     * @return filter repo
     */
    @Override
    public SquigglyFilterRepository getFilterRepository() {
        return filterRepository;
    }

    /**
     * Get the function invoker.
     *
     * @return function invoker
     */
    @Override
    public SquigglyFunctionInvoker getFunctionInvoker() {
        return functionInvoker;
    }

    /**
     * Get function repository.
     *
     * @return repo
     */
    @Override
    public SquigglyFunctionRepository getFunctionRepository() {
        return functionRepository;
    }

    /**
     * Get function security.
     *
     * @return security
     */
    @Override
    public SquigglyFunctionSecurity getFunctionSecurity() {
        return functionSecurity;
    }

    /**
     * Get the metrics.
     *
     * @return metrics
     */
    @Override
    public SquigglyMetrics getMetrics() {
        return metrics;
    }


    /**
     * Get the expression matcher.
     *
     * @return expression matcher
     */
    @Override
    public SquigglyExpressionMatcher getExpressionMatcher() {
        return nodeMatcher;
    }

    /**
     * Get the node filder.
     *
     * @return node filter
     */
    @Override
    public SquigglyNodeFilter getNodeFilter() {
        return nodeFilter;
    }

    /**
     * Get the parser.
     *
     * @return parser
     */
    @Override
    public SquigglyParser getParser() {
        return parser;
    }

    /**
     * Get the variable resolver.
     *
     * @return variable resolver
     */
    @Override
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
    public abstract static class Builder<B extends Builder<B, S>, S extends SquigglyEngine> implements SquigglyEngineInitializer<B> {

        @Nullable
        protected SquigglyConfig config;

        @Nullable
        protected SquigglyFilterContextProvider filterContextProvider;

        @Nullable
        protected Function<SquigglyConverterRegistry, SquigglyConversionService> conversionService;

        @Nullable
        protected SquigglyConverterRegistry converterRegistry;

        @Nullable
        protected SquigglyFilterRepository filterRepository;

        @Nullable
        protected SquigglyFunctionRepository functionRepository;

        @Nullable
        protected SquigglyFunctionSecurity functionSecurity;

        private final List<SquigglyFunction<?>> functions = new ArrayList<>();

        protected boolean registerDefaultConverters = true;

        protected boolean registerDefaultFunctions = true;

        private final Map<String, String> savedFilters = new HashMap<>();

        @Nullable
        private SquigglyVariableResolver variableResolver;

        private final Map<String, Object> variables = new HashMap<>();

        private List<ConverterRecord> converterRecords = new ArrayList<>();

        @Nullable
        private Function<Object, Object> serviceLocator;

        // Built Properties
        @Nullable
        protected SquigglyConfig builtConfig;

        @Nullable
        protected SquigglyFilterContextProvider builtContextProvider;

        @Nullable
        protected SquigglyConversionService builtConversionService;

        @Nullable
        protected SquigglyFilterRepository builtFilterRepository;

        @Nullable
        protected SquigglyFunctionRepository builtFunctionRepository;

        @Nullable
        protected SquigglyFunctionSecurity builtFunctionSecurity;

        @Nullable
        protected SquigglyMetrics builtMetrics;

        @Nullable
        protected SquigglyParser builtParser;

        @Nullable
        protected SquigglyVariableResolver builtVariableResolver;

        @Nullable
        protected Function<Object, Object> builtServiceLocator;

        protected Builder() {
        }

        /**
         * Add a config
         *
         * @param config the config
         * @return builder
         */
        @Override
        public B config(SquigglyConfig config) {
            this.config = config;
            return getThis();
        }

        /**
         * Sets a converter registry
         *
         * @param converterRegistry converter registry
         * @return builder
         */
        @Override
        public B converterRegistry(SquigglyConverterRegistry converterRegistry) {
            this.converterRegistry = converterRegistry;
            return getThis();
        }

        /**
         * Supply a conversion service using a factory method.
         *
         * @param factory factory method
         * @return builder
         */
        @Override
        public B conversionService(Function<SquigglyConverterRegistry, SquigglyConversionService> factory) {
            this.conversionService = factory;
            return getThis();
        }

        /**
         * Adds a converter.
         *
         * @param record converter record
         * @return builder
         */
        @Override
        public B converter(ConverterRecord record) {
            CoreAssert.notNull(record);
            converterRecords.add(record);
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
        @Override
        public <S, T> B converter(Class<S> source, Class<T> target, Function<S, T> converter) {
            this.converterRecords.add(new ConverterRecord(source, target, converter, converterRecords.size()));
            return getThis();
        }

        /**
         * Adds a converter.
         *
         * @param source    source class
         * @param target    target class
         * @param converter converter
         * @param order     order
         * @param <S>       source type
         * @param <T>       target type
         * @return builder
         */
        @Override
        public <S, T> B converter(Class<S> source, Class<T> target, Function<S, T> converter, int order) {
            this.converterRecords.add(new ConverterRecord(source, target, converter, order));
            return getThis();
        }

        /**
         * Set a context provider.
         *
         * @param contextProvider the context provider
         * @return builder
         */
        @Override
        public B filterContext(SquigglyFilterContextProvider contextProvider) {
            this.filterContextProvider = contextProvider;
            return getThis();
        }

        /**
         * Sets the filter repository.
         *
         * @param filterRepository filter repository
         * @return builder
         */
        @Override
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
        @Override
        public B functionRepository(SquigglyFunctionRepository functionRepository) {
            this.functionRepository = functionRepository;
            return getThis();
        }

        /**
         * Sets the function security.
         *
         * @param functionSecurity function security
         * @return builder
         */
        @Override
        public B functionSecurity(SquigglyFunctionSecurity functionSecurity) {
            this.functionSecurity = functionSecurity;
            return getThis();
        }

        /**
         * Register a function.
         *
         * @param function a function
         * @return builder
         */
        @Override
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
        @Override
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
        @Override
        public B functions(Iterable<SquigglyFunction<?>> functions) {
            CoreStreams.of(functions).forEach(this::function);
            return getThis();
        }

        /**
         * Registers all public static methods of the supplied classes
         *
         * @param functionClass function class
         * @return builder
         */
        @SuppressWarnings("UnnecessaryLocalVariable")
        @Override
        public B function(Class<?>... functionClass) {
            return functions(functionClass);
        }

        /**
         * Registers all public static methods of the supplied classes
         *
         * @param classes classes
         * @return builder
         */
        @SuppressWarnings("UnnecessaryLocalVariable")
        @Override
        public B functions(Class<?>... classes) {
            Object[] owners = classes;
            return functions(SquigglyFunctions.create(owners));
        }


        /**
         * Indicate whether or not to register default functions.
         *
         * @param registerDefaultConverters flag
         * @return true/false
         */
        @Override
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
        @Override
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
        @Override
        public B savedFilter(String name, String filter) {
            savedFilters.put(name, filter);
            return getThis();
        }

        /**
         * Sets a service locator.
         *
         * @param serviceLocator service locator
         * @return builder
         */
        @Override
        public B serviceLocator(Function<Object, Object> serviceLocator) {
            this.serviceLocator = serviceLocator;
            return getThis();
        }

        /**
         * Sets the variable resolver.
         *
         * @param variableResolver variable resolver
         * @return builder
         */
        @Override
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
        @Override
        public B variable(String name, Object value) {
            variables.put(name, value);
            return getThis();
        }

        /**
         * Build the squiggly object.
         *
         * @return squiggly object
         */
        public S build() {
            this.builtConfig = config == null ? new SquigglyConfig() : config;
            this.builtContextProvider = buildContextProvider();
            this.builtConversionService = buildConversionService(builtConfig);
            this.builtFilterRepository = buildFilterRepository();
            this.builtFunctionRepository = buildFunctionRepository();
            this.builtFunctionSecurity = buildFunctionSecurity();
            this.builtMetrics = new SquigglyMetrics();
            this.builtParser = new SquigglyParser(builtConfig, builtMetrics);
            this.builtVariableResolver = buildVariableResolver();
            this.builtServiceLocator = serviceLocator == null ? (key) -> null : serviceLocator;


            return newInstance();
        }

        @Nullable
        public SquigglyConfig getBuiltConfig() {
            return builtConfig;
        }

        @Nullable
        public SquigglyFilterContextProvider getBuiltContextProvider() {
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


        private SquigglyFilterContextProvider buildContextProvider() {
            SquigglyFilterContextProvider contextProvider = this.filterContextProvider;

            if (contextProvider == null) {
                contextProvider = new SimpleFilterContextProvider();
            }
            return contextProvider;
        }

        private SquigglyConversionService buildConversionService(SquigglyConfig config) {
            SquigglyConverterRegistry registry = converterRegistry == null ? new ListConverterRegistry() : converterRegistry;

            registry.addAll(converterRecords);

            if (registerDefaultConverters) {
                SystemConverters.add(registry);
            }

            if (this.conversionService == null) {
                return new PrimaryConversionService(config, registry);
            }

            return this.conversionService.apply(registry);
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

        protected SquigglyFunctionSecurity buildFunctionSecurity() {
            return functionSecurity == null ? SquigglyFunctionSecurity.ALWAYS_ALLOW : functionSecurity;
        }

        private List<SquigglyFunction<?>> getDefaultFunctions() {
            List<SquigglyFunction<?>> defaultFunctions = new ArrayList<>();
            applyDefaultFunctions(defaultFunctions);

            if (!registerDefaultFunctions) {
                Set<String> systemFunctionNames = Arrays.stream(SystemFunctionName.values())
                        .map(SystemFunctionName::getFunctionName)
                        .map(String::toLowerCase)
                        .collect(Collectors.toSet());

                defaultFunctions = defaultFunctions.stream()
                        .filter(f -> systemFunctionNames.contains(f.getName().toLowerCase())
                                || f.getAliases().stream().anyMatch(a -> systemFunctionNames.contains(a.toLowerCase())))
                        .collect(Collectors.toList());
            }

            return defaultFunctions;
        }

        protected void applyDefaultFunctions(List<SquigglyFunction<?>> functions) {
            functions.addAll(SquigglyFunctions.create(DefaultFunctions.class));
        }

        @SuppressWarnings("unchecked")
        private B getThis() {
            return (B) this;
        }
    }

}

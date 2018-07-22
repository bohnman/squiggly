package com.github.bohnman.squiggly.core;

import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.squiggly.core.bean.BeanInfoIntrospector;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.config.SystemFunctionName;
import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.convert.*;
import com.github.bohnman.squiggly.core.filter.SquigglyNodeFilter;
import com.github.bohnman.squiggly.core.filter.repository.CompositeFilterRepository;
import com.github.bohnman.squiggly.core.filter.repository.MapFilterRepository;
import com.github.bohnman.squiggly.core.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.SquigglyFunctions;
import com.github.bohnman.squiggly.core.function.functions.DefaultFunctions;
import com.github.bohnman.squiggly.core.function.invoke.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.core.function.repository.CompositeFunctionRepository;
import com.github.bohnman.squiggly.core.function.repository.MapFunctionRepository;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.function.security.SquigglyFunctionSecurity;
import com.github.bohnman.squiggly.core.match.SquigglyNodeMatcher;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.core.normalize.SquigglyNodeNormalizer;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.core.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

public abstract class BaseSquiggly {


    private final BeanInfoIntrospector beanInfoIntrospector;
    private final SquigglyConfig config;
    private final SquigglyConversionService conversionService;
    private final SquigglyContextProvider contextProvider;
    private final SquigglyFilterRepository filterRepository;
    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyFunctionSecurity functionSecurity;
    private final SquigglyMetrics metrics;
    private final SquigglyNodeMatcher nodeMatcher;
    private final SquigglyNodeNormalizer nodeNormalizer;
    private final SquigglyNodeFilter nodeFilter;
    private final SquigglyParser parser;
    private final SquigglyVariableResolver variableResolver;
    private final Function<Object, Object> serviceLocator;

    protected BaseSquiggly(BaseSquiggly.BaseBuilder builder) {
        this(builder, new BeanInfoIntrospector(builder.getBuiltConfig(), builder.getBuiltMetrics()));
    }

    @SuppressWarnings("unchecked")
    protected BaseSquiggly(BaseSquiggly.BaseBuilder builder, BeanInfoIntrospector beanInfoIntrospector) {
        this.beanInfoIntrospector = notNull(beanInfoIntrospector);
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
        this.nodeMatcher = new SquigglyNodeMatcher(this);
        this.nodeNormalizer = new SquigglyNodeNormalizer(this);
        this.nodeFilter = createNodeFilter();
        this.serviceLocator = notNull(builder.builtServiceLocator);
    }

    protected SquigglyNodeFilter createNodeFilter() {
        return new SquigglyNodeFilter(this);
    }


    public <T> CoreJsonNode<T> apply(CoreJsonNode<T> node, String... filters) {
        return nodeFilter.apply(node, filters);
    }

    /**
     * Find an object by a class or null if not found.
     *
     * @param type class of the type expected
     * @param <T>  return type expected
     * @return object or null
     */
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
    public <T> T get(Object key, Class<T> type) {
        return Objects.requireNonNull(find(key, type));
    }

    /**
     * Gets the bean info introspector
     *
     * @return bean info introspector
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
     * Get filter repo.
     *
     * @return filter repo
     */
    public SquigglyFilterRepository getFilterRepository() {
        return filterRepository;
    }

    /**
     * Get the function invoker.
     *
     * @return function invoker
     */
    public SquigglyFunctionInvoker getFunctionInvoker() {
        return functionInvoker;
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
     * Get function security.
     *
     * @return security
     */
    public SquigglyFunctionSecurity getFunctionSecurity() {
        return functionSecurity;
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
     * Get the node matcher.
     *
     * @return node matcher
     */
    public SquigglyNodeMatcher getNodeMatcher() {
        return nodeMatcher;
    }

    /**
     * Get the node normalizer.
     *
     * @return node normalizer
     */
    public SquigglyNodeNormalizer getNodeNormalizer() {
        return nodeNormalizer;
    }

    /**
     * Get the node filder.
     *
     * @return node filter
     */
    public SquigglyNodeFilter getNodeFilter() {
        return nodeFilter;
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

        @Nullable
        protected SquigglyConfig config;

        @Nullable
        protected SquigglyContextProvider contextProvider;

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
        protected SquigglyContextProvider builtContextProvider;

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

        protected BaseBuilder() {
        }

        /**
         * Add a config
         *
         * @param config the config
         * @return builder
         */
        public B config(SquigglyConfig config) {
            this.config = config;
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
         * Sets a converter registry
         *
         * @param converterRegistry converter registry
         * @return builder
         */
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
        public <S, T> B converter(Class<S> source, Class<T> target, Function<S, T> converter, int order) {
            this.converterRecords.add(new ConverterRecord(source, target, converter, order));
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
         * Sets the function security.
         *
         * @param functionSecurity function security
         * @return builder
         */
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
         * Registers all public static methods of the supplied classes
         *
         * @param functionClass function class
         * @return builder
         */
        @SuppressWarnings("UnnecessaryLocalVariable")
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
         * Sets a service locator.
         *
         * @param serviceLocator service locator
         * @return builder
         */
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

        @SuppressWarnings("unchecked")
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

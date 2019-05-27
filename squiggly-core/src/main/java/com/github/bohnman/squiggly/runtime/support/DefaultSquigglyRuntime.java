package com.github.bohnman.squiggly.runtime.support;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.convert.support.DefaultConversionService;
import com.github.bohnman.squiggly.convert.support.ListConverterRegistry;
import com.github.bohnman.squiggly.convert.support.SystemConverters;
import com.github.bohnman.squiggly.extend.SquigglyExtension;
import com.github.bohnman.squiggly.filter.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterSource;
import com.github.bohnman.squiggly.filter.support.CompositeFilterSource;
import com.github.bohnman.squiggly.filter.support.MapFilterSource;
import com.github.bohnman.squiggly.filter.support.StaticFilterProvider;
import com.github.bohnman.squiggly.function.*;
import com.github.bohnman.squiggly.function.support.CompositeFunctionSource;
import com.github.bohnman.squiggly.function.support.DefaultFunctions;
import com.github.bohnman.squiggly.function.support.MapFunctionSource;
import com.github.bohnman.squiggly.function.support.SquigglyFunctions;
import com.github.bohnman.squiggly.introspect.ObjectIntrospector;
import com.github.bohnman.squiggly.metric.support.SquigglyMetrics;
import com.github.bohnman.squiggly.parse.SquigglyParser;
import com.github.bohnman.squiggly.parse.support.AntlrSquigglyParser;
import com.github.bohnman.squiggly.property.SquigglyPropertySource;
import com.github.bohnman.squiggly.property.support.CompositePropertySource;
import com.github.bohnman.squiggly.property.support.MapPropertySource;
import com.github.bohnman.squiggly.property.support.PropertiesPropertySource;
import com.github.bohnman.squiggly.runtime.SquigglyRuntime;
import com.github.bohnman.squiggly.runtime.SquigglyRuntimeBuilder;
import com.github.bohnman.squiggly.service.SquigglyServiceSource;
import com.github.bohnman.squiggly.service.support.CompositeServiceSource;
import com.github.bohnman.squiggly.service.support.MapServiceSource;
import com.github.bohnman.squiggly.variable.SquigglyVariableSource;
import com.github.bohnman.squiggly.variable.support.CompositeVariableSource;
import com.github.bohnman.squiggly.variable.support.MapVariableSource;

import javax.annotation.Nullable;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Objects.requireNonNull;

public class DefaultSquigglyRuntime implements SquigglyRuntime {


    private final ObjectIntrospector objectIntrospector;
    private final SquigglyConfig config;
    private final SquigglyConversionService conversionService;
    private final SquigglyFilterContextProvider contextProvider;
    private final SquigglyFilterSource filterRepository;
    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyFunctionSource functionRepository;
    private final SquigglyFunctionSecurity functionSecurity;
    private final SquigglyMetrics metrics;
    private final SquigglyExpressionMatcher nodeMatcher;
    private final SquigglyParser parser;
    private final SquigglyVariableSource variableResolver;

    private final SquigglyServiceSource serviceSource;


    protected DefaultSquigglyRuntime(Builder builder) {
        this.objectIntrospector = new ObjectIntrospector(builder.getBuiltConfig(), builder.getBuiltMetrics());
        this.config = requireNonNull(builder.builtConfig);
        this.conversionService = requireNonNull(builder.builtConversionService);
        this.contextProvider = requireNonNull(builder.builtContextProvider);
        this.filterRepository = requireNonNull(builder.builtFilterRepository);
        this.functionRepository = requireNonNull(builder.builtFunctionRepository);
        this.functionSecurity = requireNonNull(builder.builtFunctionSecurity);
        this.metrics = requireNonNull(builder.builtMetrics);
        this.parser = requireNonNull(builder.builtParser);
        this.variableResolver = requireNonNull(builder.builtVariableSource);
        this.serviceSource = requireNonNull(builder.builtServiceSource);
        this.nodeMatcher = new SquigglyExpressionMatcher(getConfig(), getObjectIntrospector(), getMetrics());
        SquigglyFunctionMatcher functionMatcher = new SquigglyFunctionMatcher(conversionService);
        this.functionInvoker = new SquigglyFunctionInvoker(config, conversionService, functionMatcher, functionRepository, functionSecurity, variableResolver);
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
    public SquigglyFilterSource getFilterRepository() {
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
    public SquigglyFunctionSource getFunctionSource() {
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
    public SquigglyVariableSource getVariableResolver() {
        return variableResolver;
    }

    @Override
    public SquigglyServiceSource getServiceSource() {
        return serviceSource;
    }

    public static Builder builder() {
        return new Builder();
    }

    /**
     * Helper class that configures squiggly
     *
     * @param <B> the builder type
     * @param <S> the squiggly type
     */
    @SuppressWarnings("TypeParameterHidesVisibleType")
    public static class Builder implements SquigglyRuntimeBuilder<Builder, DefaultSquigglyRuntime> {

        @Nullable
        private SquigglyFilterContextProvider filterContextProvider;

        @Nullable
        private Function<SquigglyConverterRegistry, SquigglyConversionService> conversionService;

        @Nullable
        private SquigglyConverterRegistry converterRegistry;

        private final Map<String, String> filters = new HashMap<>();

        @Nullable
        private final List<SquigglyFilterSource> filterSources = new ArrayList<>();

        @Nullable
        private SquigglyFunctionSource functionRepository;

        @Nullable
        private SquigglyFunctionSecurity functionSecurity;

        private final List<SquigglyFunction<?>> functions = new ArrayList<>();

        protected boolean registerDefaultConverters = true;

        protected boolean registerDefaultFunctions = true;


        private final List<ConverterRecord> converterRecords = new ArrayList<>();

        private final Map<String, String> properties = new HashMap<>();

        private final List<SquigglyPropertySource> propertySources = new ArrayList<>();

        private final Map<Object, Object> services = new HashMap<>();

        private final List<SquigglyServiceSource> serviceSources = new ArrayList<>();

        private final Map<String, Object> variables = new HashMap<>();

        private final List<SquigglyVariableSource> variableSources = new ArrayList<>();


        // Built Properties
        @Nullable
        protected SquigglyConfig builtConfig;

        @Nullable
        protected SquigglyFilterContextProvider builtContextProvider;

        @Nullable
        protected SquigglyConversionService builtConversionService;

        @Nullable
        protected SquigglyFilterSource builtFilterRepository;

        @Nullable
        protected SquigglyFunctionSource builtFunctionRepository;

        @Nullable
        protected SquigglyFunctionSecurity builtFunctionSecurity;

        @Nullable
        protected SquigglyMetrics builtMetrics;

        @Nullable
        protected SquigglyParser builtParser;

        @Nullable
        protected SquigglyPropertySource builtPropertySource;

        @Nullable
        protected SquigglyVariableSource builtVariableSource;

        @Nullable
        protected SquigglyServiceSource builtServiceSource;

        protected Builder() {
        }

        @Override
        public Builder apply(SquigglyExtension extension) {
            extension.apply(this);
            return this;
        }

        @Override
        public Builder converterRegistry(SquigglyConverterRegistry converterRegistry) {
            this.converterRegistry = requireNonNull(converterRegistry);
            return this;
        }

        @Override
        public Builder conversionService(Function<SquigglyConverterRegistry, SquigglyConversionService> factory) {
            this.conversionService = requireNonNull(factory);
            return this;
        }

        @Override
        public Builder converter(ConverterRecord record) {
            converterRecords.add(requireNonNull(record));
            return this;
        }

        @Override
        public <S, T> Builder converter(Class<S> source, Class<T> target, Function<S, T> converter) {
            this.converterRecords.add(new ConverterRecord(source, target, converter, converterRecords.size()));
            return this;
        }

        @Override
        public <S, T> Builder converter(Class<S> source, Class<T> target, Function<S, T> converter, int order) {
            this.converterRecords.add(new ConverterRecord(source, target, converter, order));
            return this;
        }

        @Override
        public Builder filterContextProvider(SquigglyFilterContextProvider contextProvider) {
            this.filterContextProvider = contextProvider;
            return this;
        }

        @Override
        public Builder filter(SquigglyFilterSource source) {
            filterSources.add(requireNonNull(source));
            return this;
        }

        @Override
        public Builder function(SquigglyFunction<?> function) {
            this.functions.add(requireNonNull(function));
            return this;
        }

        @Override
        public Builder function(SquigglyFunctionSource source) {
            this.functionRepository = requireNonNull(source);
            return this;
        }

        @Override
        public Builder functionSecurity(SquigglyFunctionSecurity functionSecurity) {
            this.functionSecurity = requireNonNull(functionSecurity);
            return this;
        }

        @Override
        public Builder registerDefaultConverters(boolean registerDefaultConverters) {
            this.registerDefaultConverters = registerDefaultConverters;
            return this;
        }

        @Override
        public Builder registerDefaultFunctions(boolean registerDefaultFunctions) {
            this.registerDefaultFunctions = registerDefaultFunctions;
            return this;
        }

        @Override
        public Builder filter(String name, String filter) {
            filters.put(requireNonNull(name), requireNonNull(filter));
            return this;
        }

        @Override
        public Builder property(String name, @Nullable String value) {
            properties.put(requireNonNull(name), value);
            return this;
        }

        @Override
        public Builder property(SquigglyPropertySource source) {
            propertySources.add(requireNonNull(source));
            return this;
        }

        @Override
        public <T> Builder service(Class<T> serviceType, T service) {
            services.put(requireNonNull(serviceType), requireNonNull(service));
            return this;
        }

        @Override
        public Builder service(String name, Object service) {
            services.put(requireNonNull(name), requireNonNull(service));
            return this;
        }

        @Override
        public Builder service(SquigglyServiceSource serviceSource) {
            serviceSources.add(requireNonNull(serviceSource));
            return this;
        }

        @Override
        public Builder variable(SquigglyVariableSource source) {
            this.variableSources.add(requireNonNull(source));
            return this;
        }

        @Override
        public Builder variable(String name, @Nullable Object value) {
            variables.put(requireNonNull(name), value);
            return this;
        }

        @Override
        public DefaultSquigglyRuntime build() {
            this.builtConfig = new SquigglyConfig();
            this.builtContextProvider = buildContextProvider();
            this.builtConversionService = buildConversionService(builtConfig);
            this.builtFilterRepository = buildFilterSource();
            this.builtFunctionRepository = buildFunctionSource();
            this.builtFunctionSecurity = buildFunctionSecurity();
            this.builtMetrics = new SquigglyMetrics();
            this.builtParser = new AntlrSquigglyParser(builtConfig, builtMetrics);
            this.builtPropertySource = buildPropertySource();
            this.builtVariableSource = buildVariableSource();
            this.builtServiceSource = buildServiceSource();


            return new DefaultSquigglyRuntime(this);
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
        public SquigglyFilterSource getBuiltFilterRepository() {
            return builtFilterRepository;
        }

        @Nullable
        public SquigglyFunctionSource getBuiltFunctionRepository() {
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
        public SquigglyVariableSource getBuiltVariableSource() {
            return builtVariableSource;
        }


        protected SquigglyFilterContextProvider buildContextProvider() {
            SquigglyFilterContextProvider contextProvider = this.filterContextProvider;

            if (contextProvider == null) {
                contextProvider = new StaticFilterProvider();
            }
            return contextProvider;
        }

        protected SquigglyConversionService buildConversionService(SquigglyConfig config) {
            SquigglyConverterRegistry registry = converterRegistry == null ? new ListConverterRegistry() : converterRegistry;

            registry.addAll(converterRecords);

            if (registerDefaultConverters) {
                SystemConverters.add(registry);
            }

            if (this.conversionService == null) {
                return new DefaultConversionService(config, registry);
            }

            return this.conversionService.apply(registry);
        }


        protected SquigglyFilterSource buildFilterSource() {
            SquigglyFilterSource source = MapFilterSource.create(filters);

            if (!filterSources.isEmpty()) {
                List<SquigglyFilterSource> sources = new ArrayList<>(filterSources.size() + 1);
                sources.addAll(filterSources);
                source = CompositeFilterSource.create(sources);
            }

            return source;
        }

        protected SquigglyFunctionSource buildFunctionSource() {
            List<SquigglyFunction<?>> defaultFunctions = getDefaultFunctions();
            List<SquigglyFunctionSource> functionRepositories = new ArrayList<>(3);

            if (!functions.isEmpty()) {
                functionRepositories.add(new MapFunctionSource(functions));
            }

            if (this.functionRepository != null) {
                functionRepositories.add(this.functionRepository);
            }

            if (!defaultFunctions.isEmpty()) {
                functionRepositories.add(new MapFunctionSource(defaultFunctions));
            }

            if (functionRepositories.isEmpty()) {
                return new MapFunctionSource();
            }

            if (functionRepositories.size() == 1) {
                return functionRepositories.get(0);
            }

            return new CompositeFunctionSource(functionRepositories);
        }

        protected SquigglyFunctionSecurity buildFunctionSecurity() {
            return functionSecurity == null ? SquigglyFunctionSecurity.ALWAYS_ALLOW : functionSecurity;
        }


        protected SquigglyPropertySource buildPropertySource() {
            List<SquigglyPropertySource> sources = new ArrayList<>(propertySources.size() + 3);
            sources.add(MapPropertySource.create(properties));
            sources.addAll(propertySources);

            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

            URL squigglyProps = classLoader.getResource("squiggly.properties");

            if (squigglyProps != null) {
                sources.add(PropertiesPropertySource.create(squigglyProps));
            }

            URL squigglyDefaultProps = classLoader.getResource("squiggly.default.properties");

            if (squigglyDefaultProps != null) {
                sources.add(PropertiesPropertySource.create(squigglyDefaultProps));
            }

            return CompositePropertySource.create(sources);
        }

        protected SquigglyServiceSource buildServiceSource() {
            SquigglyServiceSource source = MapServiceSource.create(services);

            if (!serviceSources.isEmpty()) {
                List<SquigglyServiceSource> sources = new ArrayList<>(serviceSources.size() + 1);
                sources.add(source);
                sources.addAll(serviceSources);
                source = CompositeServiceSource.create(sources);
            }

            return source;
        }

        protected SquigglyVariableSource buildVariableSource() {
            SquigglyVariableSource source = MapVariableSource.create(variables);

            if (!variableSources.isEmpty()) {
                List<SquigglyVariableSource> sources = new ArrayList<>(variableSources.size() + 1);
                sources.add(source);
                sources.addAll(variableSources);
                source = CompositeVariableSource.create(sources);
            }

            return source;
        }

        protected List<SquigglyFunction<?>> getDefaultFunctions() {
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

    }
}

package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.config.source.SquigglyConfigSource;
import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;
import com.github.bohnman.squiggly.core.variable.ThreadLocalVariableResolver;
import com.github.bohnman.squiggly.jackson.Squiggly;
import com.github.bohnman.squiggly.jackson.config.SquigglyCustomizer;
import com.github.bohnman.squiggly.jackson.serialize.SquigglyJacksonSerializer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnNotWebApplication;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import javax.annotation.Nullable;
import java.util.Collection;

/**
 * Base configuration class for Squiggly.
 */
@SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
@Configuration
public class SquigglyAutoConfiguration {
    @Autowired(required = false)
    SquigglyConverterRegistry converterRegistry;

    @Autowired(required = false)
    SquigglyCustomizer customizer;

    @Autowired(required = false)
    SquigglyFilterCustomizer filterCustomizer;

    @Autowired(required = false)
    SquigglyFilterRepository filterRepository;

    @Autowired(required = false)
    SquigglyFunctionRepository functionRepository;

    @Autowired(required = false)
    SquigglyVariableResolver variableResolver;


    /**
     * Register a jackson Squiggly object.
     *
     * @param config          configuration
     * @param contextProvider context provider
     * @param serializer      serialize
     * @return Squiggly
     */
    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Bean
    @ConditionalOnMissingBean
    public Squiggly squiggly(
            SquigglyConfig config,
            SquigglyContextProvider contextProvider,
            SquigglyJacksonSerializer serializer
    ) {
        Squiggly.Builder builder = Squiggly.builder()
                .config(config)
                .context(contextProvider)
                .serializer(serializer);

        if (converterRegistry != null) {
            builder.converterRegistry(converterRegistry);
        }

        if (filterRepository != null) {
            builder.filterRepository(filterRepository);
        }

        if (functionRepository != null) {
            builder.functionRepository(functionRepository);
        }

        if (variableResolver == null) {
            builder.variableResolver(new ThreadLocalVariableResolver());
        } else {
            builder.variableResolver(variableResolver);
        }

        if (customizer != null) {
            builder = customizer.apply(builder);
        }

        return builder.build();
    }

    /**
     * Register a squiggly configuration object.
     *
     * @param environment spring boot environment
     * @return config
     */
    @Bean
    @ConditionalOnMissingBean
    public SquigglyConfig squigglyConfig(Environment environment) {
        SquigglyConfigSource configSource = new SquigglyConfigSource() {
            @Nullable
            @Override
            public String getProperty(@Nullable String name, @Nullable String defaultValue) {
                return environment.getProperty(name, defaultValue);
            }

            @Nullable
            @Override
            public String getOrigin(@Nullable String name) {
                return "Spring Environment";
            }
        };

        return new SquigglyConfig(configSource);
    }

    /**
     * Register a simple context provider with the application is not a web application.
     *
     * @param config configuration
     * @return context provider
     */
    @Bean
    @ConditionalOnMissingBean
    @ConditionalOnNotWebApplication
    public SquigglyContextProvider squigglySimpleContextProvider(SquigglyConfig config) {
        return new SimpleSquigglyContextProvider(config.getString("squiggly.spring.boot.staticFilter")) {
            @Override
            @Nullable
            protected String customizeFilter(@Nullable String filter, @Nullable Class beanClass) {
                return filterCustomizer == null ? filter : filterCustomizer.apply(filter, beanClass);
            }
        };
    }

    /**
     * Register a serializer.
     *
     * @return serializer
     */
    @Bean
    @ConditionalOnMissingBean
    public SquigglyJacksonSerializer squigglySerializer() {
        return new SquigglyJacksonSerializer() {
        };
    }

    /**
     * Register an application listener when not a web application.
     *
     * @param squiggly squiggly object
     * @return listener
     */
    @Bean
    @ConditionalOnNotWebApplication
    public static SquigglyApplicationListener squigglyApplicationListener(Squiggly squiggly) {
        return new SquigglyApplicationListener(squiggly);
    }


    /**
     * A spring listener that initializes squiggly with object mappers.
     */
    public static class SquigglyApplicationListener implements ApplicationListener<ContextRefreshedEvent> {
        private final Squiggly squiggly;
        private final Boolean autoRegister;

        public SquigglyApplicationListener(Squiggly squiggly) {
            this.squiggly = squiggly;
            this.autoRegister = squiggly.getConfig().getBoolean("squiggly.spring.boot.mapper.autoRegister", true);
        }

        @Override
        public void onApplicationEvent(ContextRefreshedEvent event) {
            Collection<ObjectMapper> objectMappers = event.getApplicationContext().getBeansOfType(ObjectMapper.class).values();
            apply(event, objectMappers);
        }

        protected void apply(ContextRefreshedEvent event, Collection<ObjectMapper> objectMappers) {
            squiggly.applyAll(objectMappers);
        }
    }
}

package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.filter.SquigglyFilterContextProvider;
import com.github.bohnman.squiggly.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.jackson.Squiggly;
import com.github.bohnman.squiggly.web.servlet.SquigglyRequest;
import com.github.bohnman.squiggly.web.servlet.SquigglyRequestFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import javax.servlet.Filter;
import java.util.Collection;

/**
 * Configuration used when the Servlet API exists in the project.
 */
@Configuration
@ConditionalOnWebApplication
@ConditionalOnClass(Filter.class)
public class SquigglyWebAutoConfiguration {

    @Autowired(required = false)
    SquigglyFilterCustomizer filterCustomizer;

    /**
     * Register a servlet filter that sets the servlet request in a thread local.
     *
     * @param config squiggly config
     * @return filter
     */
    @Bean
    public FilterRegistrationBean squigglyRequestFilter(SquigglyConfig config) {
        FilterRegistrationBean registrationBean = new FilterRegistrationBean(new SquigglyRequestFilter());
        registrationBean.setOrder(config.getInt("squiggly.spring.boot.request-filter.order", Integer.MAX_VALUE));
        return registrationBean;
    }

    /**
     * Register a context provider that looks for a filter in the servlet request.
     *
     * @param config squiggly config
     * @return provider
     */
    @Bean
    @ConditionalOnMissingBean
    public SquigglyFilterContextProvider squigglyRequestContextProvider(SquigglyConfig config) {
        return filterCustomizer == null ?
                SquigglyRequest.context(config.getFilterRequestParam())
                : SquigglyRequest.context(config.getFilterRequestParam(), filterCustomizer);
    }

    /**
     * Register an application listener.
     *
     * @param squiggly squiggly object
     * @return listener
     */
    @Bean
    public static SquigglyAutoConfiguration.SquigglyApplicationListener squigglyApplicationListener(Squiggly squiggly) {
        return new RequestSquigglyApplicationListener(squiggly);
    }

    /**
     * Application listener that sets an object mapper in the converters.
     */
    public static class RequestSquigglyApplicationListener extends SquigglyAutoConfiguration.SquigglyApplicationListener {
        public RequestSquigglyApplicationListener(Squiggly squiggly) {
            super(squiggly);
        }

        @Override
        protected void apply(ContextRefreshedEvent event, Collection<ObjectMapper> objectMappers) {
            super.apply(event, objectMappers);
            ObjectMapper objectMapper = CoreIterables.getFirst(objectMappers, null);

            // Enable Squiggly for Jackson message converter
            if (objectMapper != null) {
                Collection<MappingJackson2HttpMessageConverter> converters = event.getApplicationContext().getBeansOfType(MappingJackson2HttpMessageConverter.class).values();
                for (MappingJackson2HttpMessageConverter converter : converters) {
                    converter.setObjectMapper(objectMapper);
                }
            }
        }
    }
}

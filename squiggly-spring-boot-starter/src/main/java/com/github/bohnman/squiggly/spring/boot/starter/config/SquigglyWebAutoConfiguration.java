package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.core.collect.CoreIterables;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.web.SquigglyRequest;
import com.github.bohnman.squiggly.core.web.SquigglyRequestFilter;
import com.github.bohnman.squiggly.jackson.Squiggly;
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

@Configuration
@ConditionalOnWebApplication
@ConditionalOnClass(Filter.class)
public class SquigglyWebAutoConfiguration {

    @Autowired(required = false)
    SquigglyFilterCustomizer filterCustomizer;

    @Bean
    public FilterRegistrationBean squigglyRequestFilter(SquigglyConfig config) {
        FilterRegistrationBean registrationBean = new FilterRegistrationBean(new SquigglyRequestFilter());
        registrationBean.setOrder(config.getInt("squiggly.spring.boot.requestFilter.order", Integer.MAX_VALUE));
        return registrationBean;
    }

    @Bean
    @ConditionalOnMissingBean
    public SquigglyContextProvider squigglyRequestContextProvider(SquigglyConfig config) {
        return filterCustomizer == null ?
                SquigglyRequest.context(config.getFilterRequestParam())
                : SquigglyRequest.context(config.getFilterRequestParam(), filterCustomizer);
    }

    @Bean
    public static SquigglyAutoConfiguration.SquigglyApplicationListener squigglyApplicationListener(Squiggly squiggly) {
        return new RequestSquigglyApplicationListener(squiggly);
    }

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

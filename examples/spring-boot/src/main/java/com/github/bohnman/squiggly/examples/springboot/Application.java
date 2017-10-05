package com.github.bohnman.squiggly.examples.springboot;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.examples.springboot.web.ListResponse;
import com.github.bohnman.squiggly.web.RequestSquigglyContextProvider;
import com.github.bohnman.squiggly.web.SquigglyRequestFilter;
import com.google.common.collect.Iterables;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import javax.servlet.http.HttpServletRequest;

@SpringBootApplication
public class Application {

    @Bean
    public FilterRegistrationBean squigglyRequestFilter() {
        FilterRegistrationBean filter = new FilterRegistrationBean();
        filter.setFilter(new SquigglyRequestFilter());
        filter.setOrder(1);
        return filter;
    }

    public static void main(String[] args) throws Exception {
        ConfigurableApplicationContext context = SpringApplication.run(Application.class, args);

        Iterable<ObjectMapper> objectMappers = context.getBeansOfType(ObjectMapper.class)
            .values();

        Squiggly.init(objectMappers, new RequestSquigglyContextProvider() {
            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {

                // OPTIONAL: automatically wrap filter expressions in items{} when the object is a ListResponse
                if (filter != null && ListResponse.class.isAssignableFrom(beanClass)) {
                    filter = "items[" + filter + "]";
                }

                return filter;
            }
        });

        ObjectMapper objectMapper = Iterables.getFirst(objectMappers, null);

        // Enable Squiggly for Jackson message converter
        if (objectMapper != null) {
            for (MappingJackson2HttpMessageConverter converter : context.getBeansOfType(MappingJackson2HttpMessageConverter.class).values()) {
                converter.setObjectMapper(objectMapper);
            }
        }
    }

}

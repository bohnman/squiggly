package com.github.bohnman.examples.springdatarest;

import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.web.RequestSquigglyContextProvider;
import com.github.bohnman.squiggly.web.SquigglyRequestFilter;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

@SpringBootApplication
public class Application {

    @Bean
    public FilterRegistrationBean squigglyRequestFilter() {
        FilterRegistrationBean<SquigglyRequestFilter> filter = new FilterRegistrationBean<>();
        filter.setFilter(new SquigglyRequestFilter());
        filter.setOrder(1);
        return filter;
    }


    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(Application.class, args);

        SquigglyContextProvider contextProvider = new RequestSquigglyContextProvider();

        context.getBeansOfType(MappingJackson2HttpMessageConverter.class)
                .values()
                .stream()
                .map(MappingJackson2HttpMessageConverter::getObjectMapper)
                .forEach(objectMapper -> Squiggly.init(objectMapper, contextProvider));
    }
}

package com.github.bohnman.examples.springdatarest;

import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.web.RequestSquigglyContextProvider;
import com.github.bohnman.squiggly.web.SquigglyRequestFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.rest.core.config.RepositoryRestConfiguration;
import org.springframework.data.rest.webmvc.config.RepositoryRestConfigurerAdapter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import javax.persistence.EntityManager;
import javax.persistence.metamodel.Type;

@SpringBootApplication
public class Application {

    @Bean
    public FilterRegistrationBean squigglyRequestFilter() {
        FilterRegistrationBean<SquigglyRequestFilter> filter = new FilterRegistrationBean<>();
        filter.setFilter(new SquigglyRequestFilter());
        filter.setOrder(1);
        return filter;
    }

    @Configuration
    public class RestConfig extends RepositoryRestConfigurerAdapter {

        private final EntityManager entityManager;

        @Autowired
        public RestConfig(EntityManager entityManager) {
            this.entityManager = entityManager;
        }

        @Override
        public void configureRepositoryRestConfiguration(RepositoryRestConfiguration config) {
            config.exposeIdsFor(
                    entityManager.getMetamodel().getEntities().stream()
                            .map(Type::getJavaType)
                            .toArray(Class[]::new));
        }
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

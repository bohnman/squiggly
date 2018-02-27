package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.web.SquigglyRequest;
import com.github.bohnman.squiggly.core.web.SquigglyRequestFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;

@ConditionalOnWebApplication
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
    @ConditionalOnWebApplication
    public SquigglyContextProvider squigglyRequestContextProvider() {
        return filterCustomizer == null ? SquigglyRequest.context() : SquigglyRequest.context(filterCustomizer);
    }
}

package com.github.bohnman.squiggly.examples.springboot.webfluxfunctional;


import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.filter.SquilggyFilterCustomizers;
import com.github.bohnman.squiggly.examples.springboot.webfluxfunctional.web.IssueHandler;
import com.github.bohnman.squiggly.examples.springboot.webfluxfunctional.web.ListResponse;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;

import static org.springframework.web.reactive.function.server.RequestPredicates.GET;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@SpringBootApplication
public class Application {

    @Bean
    public SquigglyFilterCustomizer squigglyFilterCustomizer() {
        return SquilggyFilterCustomizers.wrap(ListResponse.class, "items[", "]");
    }

    @SuppressWarnings("NullableProblems")
    @Bean
    public RouterFunction<ServerResponse> issueRouter(IssueHandler handler) {
        return route(GET("/issues"), handler::findAll)
                .andRoute(GET("/issues/{id}"), handler::findById);
    }

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

}

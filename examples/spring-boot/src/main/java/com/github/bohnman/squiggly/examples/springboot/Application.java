package com.github.bohnman.squiggly.examples.springboot;


import com.github.bohnman.squiggly.core.filter.SquiggilyFilterCustomizers;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.examples.springboot.web.ListResponse;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
//@EnableAutoConfiguration(exclude = SquigglyWebFluxAutoConfiguration.class)
public class Application {

    @Bean
    public SquigglyFilterCustomizer squigglyFilterCustomizer() {
        return SquiggilyFilterCustomizers.wrap(ListResponse.class, "items[", "]");
    }

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

}

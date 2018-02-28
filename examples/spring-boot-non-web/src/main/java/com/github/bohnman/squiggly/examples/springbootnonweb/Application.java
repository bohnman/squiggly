package com.github.bohnman.squiggly.examples.springbootnonweb;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.examples.springbootnonweb.model.Issue;
import com.github.bohnman.squiggly.jackson.Squiggly;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.IOException;

@SpringBootApplication
public class Application {

    public static void main(String[] args) throws IOException {
        System.setProperty("squiggly.spring.boot.staticFilter", args.length == 0 ? "id" : args[0]);
        SpringApplication springApplication = new SpringApplication(Application.class);
        springApplication.setWebEnvironment(false);
        ConfigurableApplicationContext context = springApplication.run(args);
        Squiggly squiggly = context.getBean(Squiggly.class);

        System.out.print("\n\n\n");
        System.out.println("RESULT:");
        String result = squiggly.apply(new ObjectMapper()).writeValueAsString(Issue.findAll());
        System.out.println(result);
        System.out.print("\n\n\n");
    }

}

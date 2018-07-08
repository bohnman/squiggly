package com.github.bohnman.squiggly.examples.springboot.webfluxfunctional.web;

import com.github.bohnman.squiggly.examples.springboot.webfluxfunctional.model.Issue;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import static org.springframework.web.reactive.function.BodyInserters.fromObject;

@Component
public class IssueHandler {

    public Mono<ServerResponse> findAll(ServerRequest request) {
        return ServerResponse.ok().body(fromObject(ListResponse.of(Issue.findAll())));
    }

    public Mono<ServerResponse> findById(ServerRequest request) {
        String id = request.pathVariable("id");
        for (Issue issue : Issue.findAll()) {
            if (issue.getId().equals(id)) {
                return ServerResponse.ok().body(fromObject(issue));
            }
        }

        return ServerResponse.status(HttpStatus.NOT_FOUND).body(fromObject("Not Found"));
    }
}

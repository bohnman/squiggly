package com.github.bohnman.squiggly.examples.springboot.web;

import com.github.bohnman.squiggly.examples.springboot.exception.NotFoundException;
import com.github.bohnman.squiggly.examples.springboot.model.ErrorResponse;
import com.github.bohnman.squiggly.examples.springboot.model.Issue;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

@RestController
public class IssueController {

    @RequestMapping("/issues")
    public ListResponse<Issue> findAll() {
        return ListResponse.of(Issue.findAll());
    }

    @RequestMapping("/issues/{id}")
    public Issue findById(@PathVariable String id) {
        for (Issue issue : Issue.findAll()) {
            if (issue.getId().equals(id)) {
                return issue;
            }
        }

        throw new NotFoundException(String.format("Issue %s was not found.", id));
    }


    @ExceptionHandler(NotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    public ErrorResponse onNotFound(NotFoundException exception) {
        return new ErrorResponse(exception.getMessage());
    }
}

package com.github.bohnman.squiggly.examples.springboot.web;

import com.github.bohnman.squiggly.examples.springboot.model.Issue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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

        return null;
    }
}

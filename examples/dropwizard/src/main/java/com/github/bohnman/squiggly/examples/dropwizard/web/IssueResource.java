package com.github.bohnman.squiggly.examples.dropwizard.web;

import com.github.bohnman.squiggly.examples.dropwizard.model.Issue;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/issues")
@Produces(MediaType.APPLICATION_JSON)
public class IssueResource {

    @GET
    public ListResponse<Issue> findAll() {
        return ListResponse.of(Issue.findAll());
    }

    @GET
    @Path("/{id}")
    public Issue findById(@PathParam("id") String id) {
        for (Issue issue : Issue.findAll()) {
            if (issue.getId().equals(id)) {
                return issue;
            }
        }

        return null;
    }
}

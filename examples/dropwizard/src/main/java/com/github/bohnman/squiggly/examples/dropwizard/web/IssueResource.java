package dev.nicklasw.squiggly.examples.dropwizard.web;

import dev.nicklasw.squiggly.examples.dropwizard.model.Issue;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

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

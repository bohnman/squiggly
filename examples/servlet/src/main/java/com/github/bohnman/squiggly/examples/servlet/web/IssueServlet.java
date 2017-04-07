package com.github.bohnman.squiggly.examples.servlet.web;

import com.github.bohnman.squiggly.examples.servlet.model.Issue;
import com.github.bohnman.squiggly.examples.servlet.util.Jackson;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class IssueServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String pathInfo = req.getPathInfo();

        if (pathInfo == null || pathInfo.equals("") || pathInfo.equals("/")) {
            findAll(req, resp);
        } else {
            findById(pathInfo.substring(1), req, resp);
        }
    }


    private void findAll(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        ListResponse<Issue> issues = ListResponse.of(Issue.findAll());
        String json = Jackson.objectMapper().writeValueAsString(issues);

        resp.setContentType("application/json");
        resp.setContentLength(json.length());

        resp.getWriter().print(json);
    }


    private void findById(String id, HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        Issue issue = findById(id);

        if (issue == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        String json = Jackson.objectMapper().writeValueAsString(issue);

        resp.setContentType("application/json");
        resp.setContentLength(json.length());

        resp.getWriter().print(json);
    }

    private Issue findById(String id) {
        for (Issue issue : Issue.findAll()) {
            if (issue.getId().equals(id)) {
                return issue;
            }
        }

        return null;
    }
}

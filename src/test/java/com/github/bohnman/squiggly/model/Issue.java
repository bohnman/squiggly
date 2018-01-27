package com.github.bohnman.squiggly.model;

import com.github.bohnman.squiggly.view.FullView;
import com.github.bohnman.squiggly.view.PropertyView;

import java.util.List;
import java.util.Map;

public class Issue extends BaseEntity {

    private String issueSummary;
    private String issueDetails;
    private User reporter;
    private User assignee;

    @FullView
    @OtherView
    private List<IssueAction> actions;

    @PropertyView("view1")
    @FullView
    private Map<String, Object> properties;


    public String getIssueSummary() {
        return issueSummary;
    }

    public void setIssueSummary(String issueSummary) {
        this.issueSummary = issueSummary;
    }

    public String getIssueDetails() {
        return issueDetails;
    }

    public void setIssueDetails(String issueDetails) {
        this.issueDetails = issueDetails;
    }

    public User getReporter() {
        return reporter;
    }

    public void setReporter(User reporter) {
        this.reporter = reporter;
    }

    public User getAssignee() {
        return assignee;
    }

    public void setAssignee(User assignee) {
        this.assignee = assignee;
    }


    public List<IssueAction> getActions() {
        return actions;
    }

    public void setActions(List<IssueAction> actions) {
        this.actions = actions;
    }


    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }
}

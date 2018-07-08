package com.github.bohnman.squiggly.examples.standalone.model;

import com.github.bohnman.squiggly.core.view.FullView;
import com.github.bohnman.squiggly.core.view.PropertyView;

import java.util.*;

public class Issue extends BaseIssue {

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

    public static List<Issue> findAll() {
        return Arrays.asList(buildIssue1(), buildIssue2(), buildIssue3());
    }

    private static Issue buildIssue1() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("email", "motherofdragons@got.com");
        properties.put("priority", "1");

        Issue issue = new Issue();
        issue.setId("ISSUE-1");
        issue.setIssueSummary("Dragons Need Fed");
        issue.setIssueDetails("I need my dragons fed pronto.");
        User assignee = new User("Jorah", "Mormont");
        issue.setAssignee(assignee);
        issue.setReporter(new User("Daenerys", "Targaryen"));
        issue.setActions(Arrays.asList(
                new IssueAction("COMMENT", "I'm going to let Daario get this one..", assignee),
                new IssueAction("CLOSE", "All set.", new User("Daario", "Naharis"))
        ));
        issue.setProperties(properties);
        return issue;
    }

    private static Issue buildIssue2() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("email", "tlannister@got.com");
        properties.put("priority", "5");

        Issue issue = new Issue();
        issue.setId("ISSUE-2");
        issue.setIssueSummary("More wine please!");
        issue.setIssueDetails("I need more wine.  I've only had 3 bottles so far.");
        User assignee = new User("Joffrey", "Baratheon");
        issue.setAssignee(assignee);
        issue.setReporter(new User("Tyrion", "Lannister"));
        issue.setActions(Collections.singletonList(
                new IssueAction("COMMENT", "Ugh.  Do have to?", assignee)
        ));
        issue.setProperties(properties);
        return issue;
    }

    private static Issue buildIssue3() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("email", "rambo@got.com");
        properties.put("priority", "7");

        Issue issue = new Issue();
        issue.setId("ISSUE-3");
        issue.setIssueSummary("Hello Kitty pillow missing");
        issue.setIssueDetails("Can't seem to find my Hello Kitty pillow.  Did I feed it to the dogs?");
        User assignee = new User("Theon", "Greyjoy");
        issue.setAssignee(assignee);
        issue.setReporter(new User("Ramsay", "Bolton"));
        issue.setActions(Collections.singletonList(
                new IssueAction("CLOSE", "Your wish is my command.", assignee)
        ));
        issue.setProperties(properties);
        return issue;
    }

}

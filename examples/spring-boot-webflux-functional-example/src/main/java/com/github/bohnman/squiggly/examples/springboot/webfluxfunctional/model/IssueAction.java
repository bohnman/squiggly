package com.github.bohnman.squiggly.examples.springboot.webfluxfunctional.model;

import com.github.bohnman.squiggly.core.view.FullView;

public class IssueAction extends BaseIssue {

    private String type;
    private String text;

    @FullView
    private User user;


    public IssueAction(String type, String text, User user) {
        this.text = text;
        this.type = type;
        this.user = user;
    }


    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }
}

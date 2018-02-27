package com.github.bohnman.squiggly.jackson.model;

import com.fasterxml.jackson.annotation.JsonUnwrapped;

public class Outer {

    private String outerText;

    private Inner inner;

    public Outer() {
    }

    public Outer(String outerText, String inner) {
        this.outerText = outerText;
        this.inner = new Inner(inner);
    }

    public String getOuterText() {
        return outerText;
    }

    public void setOuterText(String outerText) {
        this.outerText = outerText;
    }

    public Inner getInner() {
        return inner;
    }

    @JsonUnwrapped
    public void setInner(Inner inner) {
        this.inner = inner;
    }
}

package com.github.bohnman.squiggly.cli.printer;

import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;

public class SyntaxHighlightingPrettyPrinter extends DefaultPrettyPrinter {

    private final SyntaxHighlighter syntaxHighlighter;

    public SyntaxHighlightingPrettyPrinter(SyntaxHighlighter syntaxHighlighter) {
        this.syntaxHighlighter = syntaxHighlighter;
    }

    public SyntaxHighlighter getSyntaxHighlighter() {
        return syntaxHighlighter;
    }

}
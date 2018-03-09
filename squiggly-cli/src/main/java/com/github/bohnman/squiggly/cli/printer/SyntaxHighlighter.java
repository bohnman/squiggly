package com.github.bohnman.squiggly.cli.printer;

public class SyntaxHighlighter {

    public String getBooleanColor() {
        return "";
    }

    public String getFieldNameColor() {
        return Ansi.CYAN;
    }

    public String getNullColor() {
        return "";
    }

    public String getNumberColor() {
        return "";
    }

    public String getStringColor() {
        return Ansi.GREEN;
    }

    public String getReset() {
        return Ansi.RESET;
    }
}

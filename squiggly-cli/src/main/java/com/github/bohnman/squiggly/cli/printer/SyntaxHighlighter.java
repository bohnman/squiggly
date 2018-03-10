package com.github.bohnman.squiggly.cli.printer;

import com.github.bohnman.squiggly.core.config.SquigglyConfig;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

public class SyntaxHighlighter {

    private final String booleanColor;
    private final String fieldNameColor;
    private final String nullColor;
    private final String numberColor;
    private final String stringColor;

    public SyntaxHighlighter(SquigglyConfig config) {
        this.booleanColor = parseColor(config, "boolean");
        this.fieldNameColor = parseColor(config, "fieldName");
        this.nullColor = parseColor(config, "null");
        this.numberColor = parseColor(config, "name");
        this.stringColor = parseColor(config, "string");
    }

    public String getBooleanColor() {
        return booleanColor;
    }

    public String getFieldNameColor() {
        return fieldNameColor;
    }

    public String getNullColor() {
        return nullColor;
    }

    public String getNumberColor() {
        return numberColor;
    }

    public String getStringColor() {
        return stringColor;
    }

    public String getReset() {
        return Ansi.RESET.getCode();
    }

    private static String parseColor(SquigglyConfig config, String name) {
        String value = config.getString("squiggly.cli.color." + name, "");
        return parseColor(value);
    }

    private static String parseColor(String value) {
        if (value.isEmpty()) {
            return "";
        }

        return String.join("", Arrays.stream(value.split(";"))
                .map(SyntaxHighlighter::toAnsi)
                .filter(Objects::nonNull)
                .map(Ansi::getCode)
                .collect(Collectors.toList())
        );
    }

    private static Ansi toAnsi(String value) {
        try {
            return Ansi.valueOf(value.trim().toUpperCase());
        } catch (IllegalArgumentException e) {
            return null;
        }
    }


}

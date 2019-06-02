package com.github.bohnman.squiggly.cli.text;

import com.github.bohnman.squiggly.environment.SquigglyEnvironment;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Colorizes output on the command line.
 */
public class SyntaxHighlighter {

    private final String booleanColor;
    private final String fieldNameColor;
    private final String nullColor;
    private final String numberColor;
    private final String stringColor;

    public SyntaxHighlighter(SquigglyEnvironment config) {
        this.booleanColor = parseColor(config, "boolean");
        this.fieldNameColor = parseColor(config, "field-name");
        this.nullColor = parseColor(config, "null");
        this.numberColor = parseColor(config, "name");
        this.stringColor = parseColor(config, "string");
    }

    /**
     * Gets the color for boolean literals.
     *
     * @return color
     */
    public String getBooleanColor() {
        return booleanColor;
    }

    /**
     * Gets the color for field names.
     *
     * @return color
     */
    public String getFieldNameColor() {
        return fieldNameColor;
    }

    /**
     * Gets the color for null keyword.
     *
     * @return color
     */
    public String getNullColor() {
        return nullColor;
    }

    /**
     * Gets the color for number literals.
     *
     * @return color
     */
    public String getNumberColor() {
        return numberColor;
    }

    /**
     * Gets the color for string literals.
     *
     * @return color
     */
    public String getStringColor() {
        return stringColor;
    }

    /**
     * Gets the ansi reset code.
     *
     * @return code
     */
    public String getReset() {
        return Ansi.RESET.getCode();
    }

    private static String parseColor(SquigglyEnvironment config, String name) {
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

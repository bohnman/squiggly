package com.github.bohnman.squiggly.core.name;

import java.util.regex.Pattern;

/**
 * Represents a regex name match.
 */
public class RegexName implements SquigglyName {

    private final String name;
    private final String rawName;
    private final Pattern pattern;

    /**
     * Constructor.
     *
     * @param name    the raw pattern string
     * @param pattern the compiled regex
     */
    public RegexName(String name, Pattern pattern) {
        this.name = name;
        this.rawName = name;
        this.pattern = pattern;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getRawName() {
        return rawName;
    }

    @Override
    public int match(String name) {
        if (pattern.matcher(name).matches()) {
            return rawName.length() + 2;
        }

        return -1;
    }
}

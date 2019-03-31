package com.github.bohnman.squiggly.core.name;

import java.util.regex.Pattern;

/**
 * Represents a regex name match.
 */
public class RegexName extends BaseSquigglyName {

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
    public int getSpecificity() {
        return rawName.length() + 2;
    }

    @Override
    public boolean matches(String name) {
        return pattern.matcher(name).matches();
    }
}

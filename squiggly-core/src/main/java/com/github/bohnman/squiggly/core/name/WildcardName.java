package com.github.bohnman.squiggly.core.name;

import com.github.bohnman.core.lang.CoreStrings;

import java.util.regex.Pattern;

/**
 * Represents a wildcard match.  For example: foo*
 */
public class WildcardName implements SquigglyName {

    private final String name;
    private final String rawName;
    private final Pattern pattern;

    /**
     * Constructor.
     *
     * @param name wildcard string
     */
    public WildcardName(String name) {
        this.name = name;
        this.rawName = CoreStrings.remove(this.name, "*");
        this.pattern = buildPattern();
    }

    private Pattern buildPattern() {
        String[] search = {"*", "?"};
        String[] replace = {".*", ".?"};

        return Pattern.compile("^" + CoreStrings.replaceEach(name, search, replace) + "$");
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getRawName() {
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

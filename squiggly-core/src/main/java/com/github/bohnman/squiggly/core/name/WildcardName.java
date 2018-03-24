package com.github.bohnman.squiggly.core.name;

import com.github.bohnman.core.lang.CoreStrings;

import java.util.regex.Pattern;

public class WildcardName implements SquigglyName {

    private final String name;
    private final String rawName;
    private final Pattern pattern;

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
    public int match(String name) {
        if (pattern.matcher(name).matches()) {
            return rawName.length() + 2;
        }

        return -1;
    }

    @Override
    public String toString() {
        return rawName;
    }
}

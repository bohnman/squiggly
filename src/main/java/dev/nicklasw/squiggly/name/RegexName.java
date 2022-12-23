package dev.nicklasw.squiggly.name;

import java.util.Set;
import java.util.regex.Pattern;

public class RegexName implements SquigglyName {

    private final String name;
    private final String rawName;
    private final Pattern pattern;

    public RegexName(String name, Set<String> flags) {
        this.name = name;
        this.rawName = name;
        this.pattern = buildPattern(name, flags);
    }

    private Pattern buildPattern(String name, Set<String> flags) {
        int flagMask = 0;

        if (flags != null && !flags.isEmpty()) {
            for (String flag : flags) {
                switch (flag) {
                    case "i":
                        flagMask |= Pattern.CASE_INSENSITIVE;
                        break;
                    default:
                        throw new IllegalArgumentException("Unrecognized flag " + flag + " for pattern " + name);
                }
            }
        }

        return Pattern.compile(name, flagMask);
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

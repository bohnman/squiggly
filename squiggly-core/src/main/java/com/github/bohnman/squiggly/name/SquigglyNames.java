package com.github.bohnman.squiggly.name;

import com.github.bohnman.core.lang.CoreStrings;

import java.util.regex.Pattern;

import static java.util.Objects.requireNonNull;

public class SquigglyNames {

    public static final String ANY_DEEP_TOKEN = "**";
    public static final String ANY_SHALLOW_TOKEN = "*";
    public static final String DEEP_INHERIT_TOKEN = "...";
    public static final String NEVER_MATCH_TOKEN = "__NEVER__";


    private static final AnyDeepName ANY_DEEP = new AnyDeepName();
    private static final AnyShallowName ANY_SHALLOW = new AnyShallowName();
    private static final DeepInheritName DEEP_INHERIT = new DeepInheritName();
    private static final NeverMatchName NEVER_MATCH = new NeverMatchName();

    private SquigglyNames() {
    }

    public static SquigglyName anyDeep() {
        return ANY_DEEP;
    }

    public static SquigglyName anyShallow() {
        return ANY_SHALLOW;
    }

    public static SquigglyName deepInherit() {
        return DEEP_INHERIT;
    }

    public static SquigglyName exact(String name) {
        return new ExactName(name);
    }

    public static SquigglyName neverMatch() {
        return NEVER_MATCH;
    }

    public static SquigglyName regex(String regex) {
        return new RegexName(regex, Pattern.compile(regex));
    }

    public static SquigglyName regex(String regex, Pattern pattern) {
        return new RegexName(regex, pattern);
    }

    public static SquigglyName wildcard(String wildcard) {
        return new WildcardName(wildcard);
    }


    /**
     * Indicates that the node name matches any node at any nesting level.
     */
    private static class AnyDeepName extends BaseSquigglyName {

        private AnyDeepName() {
        }

        @Override
        public String getToken() {
            return ANY_DEEP_TOKEN;
        }

        @Override
        public int getSpecificity() {
            return 0;
        }

        @Override
        public boolean matches(String token) {
            return true;
        }
    }

    /**
     * Represent a name that matches any node at the current nesting level.
     */
    private static class AnyShallowName extends BaseSquigglyName {

        private AnyShallowName() {
        }

        @Override
        public String getToken() {
            return ANY_SHALLOW_TOKEN;
        }

        @Override
        public int getSpecificity() {
            return 1;
        }

        @Override
        public boolean matches(String token) {
            return true;
        }
    }

    private static class DeepInheritName extends BaseSquigglyName {

        private DeepInheritName() {
        }

        @Override
        public String getToken() {
            return DEEP_INHERIT_TOKEN;
        }

        @Override
        public int getSpecificity() {
            return 0;
        }

        @Override
        public boolean matches(String token) {
            return false;
        }
    }

    /**
     * Represents an exact name match.
     */
    public static class ExactName extends BaseSquigglyName {

        private final String token;

        /**
         * Constructor.
         *
         * @param token the extact token
         */
        ExactName(String token) {
            this.token = requireNonNull(token);
        }

        @Override
        public String getToken() {
            return token;
        }

        @Override
        public int getSpecificity() {
            return Integer.MAX_VALUE;
        }

        @Override
        public boolean matches(String token) {
            return this.token.equals(token);
        }
    }

    /**
     * Indicates that the node name matches any node at any nesting level.
     */
    public static class NeverMatchName extends BaseSquigglyName {

        @Override
        public String getToken() {
            return NEVER_MATCH_TOKEN;
        }

        @Override
        public int getSpecificity() {
            return 0;
        }

        @Override
        public boolean matches(String token) {
            return false;
        }
    }

    /**
     * Represents a regex name match.
     */
    public static class RegexName extends BaseSquigglyName {

        private final String token;
        private final String rawName;
        private final Pattern pattern;

        /**
         * Constructor.
         *
         * @param token   the raw pattern string
         * @param pattern the compiled regex
         */
        public RegexName(String token, Pattern pattern) {
            this.token = token;
            this.rawName = token;
            this.pattern = pattern;
        }

        @Override
        public String getToken() {
            return token;
        }

        @Override
        public int getSpecificity() {
            return rawName.length() + 2;
        }

        @Override
        public boolean matches(String token) {
            return pattern.matcher(token).matches();
        }
    }

    /**
     * Represents a wildcard match.  For example: foo*
     */
    public static class WildcardName extends BaseSquigglyName {

        private final String token;
        private final String rawName;
        private final Pattern pattern;

        /**
         * Constructor.
         *
         * @param token wildcard string
         */
        public WildcardName(String token) {
            this.token = token;
            this.rawName = CoreStrings.remove(this.token, "*");
            this.pattern = buildPattern();
        }

        private Pattern buildPattern() {
            String[] search = {"*", "?"};
            String[] replace = {".*", ".?"};

            return Pattern.compile("^" + CoreStrings.replaceEach(token, search, replace) + "$");
        }

        @Override
        public String getToken() {
            return token;
        }

        @Override
        public int getSpecificity() {
            return rawName.length() + 2;
        }

        @Override
        public boolean matches(String token) {
            return pattern.matcher(token).matches();
        }
    }
}

package com.github.bohnman.squiggly.parser;

import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;

import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode {

    public static final String ANY_DEEP = "**";
    public static final String ANY_SHALLOW = "*";

    private final String name;
    private final SquigglyNode parent;
    private final List<SquigglyNode> children;
    private final boolean squiggly;
    private final Pattern pattern;
    private final boolean negated;

    /**
     * Constructor.
     *
     * @param name     name of the node
     * @param parent   parent node
     * @param children child nodes
     * @param squiggly whether or not a not is squiggly
     * @see #isSquiggly()
     */
    public SquigglyNode(String name, SquigglyNode parent, List<SquigglyNode> children, boolean squiggly) {
        Validate.isTrue(StringUtils.isNotEmpty(name), "Node names must not be empty");
        Validate.isTrue(!name.equals("-"), "Illegal node name '-'");

        if (name.charAt(0) == '-') {
            this.name = name.substring(1);
            this.negated = true;
        } else {
            this.name = name;
            this.negated = false;
        }

        this.parent = parent;
        this.children = Collections.unmodifiableList(children);
        this.squiggly = squiggly;
        this.pattern = buildPattern();
    }

    private Pattern buildPattern() {
        if (isAnyShallow()) {
            return null;
        }

        if (isAnyDeep()) {
            return null;
        }

        if (!StringUtils.containsAny(name, "*?")) {
            return null;
        }

        String[] search = {"*", "?"};
        String[] replace = {".*", ".?"};

        return Pattern.compile("^" + StringUtils.replaceEach(name, search, replace) + "$");
    }

    /**
     * Performs a match against the name of another node/element.
     *
     * @param otherName the name of the other node
     * @return -1 if no match, 0 if exact match, or positive number for wildcards
     */
    public int match(String otherName) {

        if (pattern != null) {
            if (pattern.matcher(otherName).matches()) {
                return name.length();
            } else {
                return -1;
            }
        }

        if (isAnyDeep()) {
            return Integer.MAX_VALUE;
        }

        if (isAnyShallow()) {
            return Integer.MAX_VALUE - 1;
        }

        if (name.equals(otherName)) {
            return 0;
        }

        return -1;
    }

    /**
     * Get the name of the node.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the node's parent.
     *
     * @return parent node
     */
    public SquigglyNode getParent() {
        return parent;
    }

    /**
     * Get the node's children.
     *
     * @return child nodes
     */
    public List<SquigglyNode> getChildren() {
        return children;
    }

    /**
     * A node is considered squiggly if it is comes right before a nested expression.
     * <p>For example, given the filter expression:</p>
     * <code>id,foo{bar}</code>
     * <p>The foo node is squiggly, but the bar node is not.</p>
     *
     * @return true/false
     */
    public boolean isSquiggly() {
        return squiggly;
    }

    /**
     * Says whether this node is **
     *
     * @return true if **, false if not
     */
    public boolean isAnyDeep() {
        return ANY_DEEP.equals(name);
    }

    /**
     * Says whehter this node is *
     *
     * @return true if *, false if not
     */
    public boolean isAnyShallow() {
        return ANY_SHALLOW.equals(name);
    }

    /**
     * Says whether the node started with '-'.
     *
     * @return true if negated false if not
     */
    public boolean isNegated() {
        return negated;
    }
}

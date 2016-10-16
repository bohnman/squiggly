package com.github.bohnman.squiggly.parser;

import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.StringUtils;

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
        this.name = name;
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
     * Whether this node's name matches against another node's name.
     *
     * @param otherName the name of the other node
     * @return true/false
     */
    public boolean nameMatches(String otherName) {

        if (pattern != null) {
            return pattern.matcher(otherName).matches();
        }

        if (isAnyDeep()) {
            return true;
        }

        if (isAnyShallow()) {
            return true;
        }

        return name.equals(otherName);
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
     * <p>
     * <p>For example, given the filter expression:</p>
     * <p>
     * <code>id,foo{bar}</code>
     * <p>
     * <p>The foo node is squiggly, but the bar node is not.</p>
     *
     * @return true/false
     */
    public boolean isSquiggly() {
        return squiggly;
    }

    public boolean isAnyDeep() {
        return ANY_DEEP.equals(name);
    }

    public boolean isAnyShallow() {
        return ANY_SHALLOW.equals(name);
    }
}

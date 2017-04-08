package com.github.bohnman.squiggly.parser;

import com.github.bohnman.squiggly.name.AnyDeepName;
import com.github.bohnman.squiggly.name.AnyShallowName;
import com.github.bohnman.squiggly.name.SquigglyName;
import net.jcip.annotations.ThreadSafe;

import java.util.Collections;
import java.util.List;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode {

    private final SquigglyName name;
    private final SquigglyNode parent;
    private final List<SquigglyNode> children;
    private final boolean squiggly;
    private final boolean negated;
    private final boolean emptyNested;

    /**
     * Constructor.
     *
     * @param name     name of the node
     * @param parent   parent node
     * @param children child nodes
     * @param negated whether or not the node has been negated
     * @param squiggly whether or not a not is squiggly
     * @param emptyNested whether of not filter specified {}
     * @see #isSquiggly()
     */
    public SquigglyNode(SquigglyName name, SquigglyNode parent, List<SquigglyNode> children, boolean negated, boolean squiggly, boolean emptyNested) {
        this.name = name;
        this.negated = negated;
        this.parent = parent;
        this.children = Collections.unmodifiableList(children);
        this.squiggly = squiggly;
        this.emptyNested = emptyNested;
    }

    /**
     * Performs a match against the name of another node/element.
     *
     * @param otherName the name of the other node
     * @return -1 if no match, MAX_INT if exact match, or positive number for wildcards
     */
    public int match(String otherName) {
        return name.match(otherName);
    }

    /**
     * Get the name of the node.
     *
     * @return name
     */
    public String getName() {
        return name.getName();
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
        return AnyDeepName.ID.equals(name.getName());
    }

    /**
     * Says whehter this node is *
     *
     * @return true if *, false if not
     */
    public boolean isAnyShallow() {
        return AnyShallowName.ID.equals(name.getName());
    }

    /**
     * Says whether this node  explicity specified no children.  (eg. assignee{})
     *
     * @return true if empty nested, false otherwise
     */
    public boolean isEmptyNested() {
        return emptyNested;
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

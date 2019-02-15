package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.name.*;
import com.github.bohnman.squiggly.core.parser.ParseContext;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Collections;
import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode implements Comparable<SquigglyNode> {

    public static final String ROOT = "root";
    public static final SquigglyNode EMPTY = SquigglyNode.createNamed(new ExactName(ROOT));

    private final ParseContext context;
    private final SquigglyName name;
    private final List<SquigglyNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final boolean squiggly;
    private final boolean negated;
    private final boolean emptyNested;
    private final boolean deep;
    private final Integer minDepth;
    private final Integer maxDepth;
    private final int stage;

    /**
     * Constructor.
     *
     * @param context        parser context
     * @param name           name of the node
     * @param children       child nodes
     * @param stage          stage
     * @param keyFunctions   key functions
     * @param valueFunctions value functions
     * @param negated        whether or not the node has been negated
     * @param squiggly       whether or not a node is squiggly
     * @param emptyNested    whether of not filter specified {}
     * @param deep           whether or not this node is deep
     * @param minDepth       min depth
     * @param maxDepth       max depth
     * @see #isSquiggly()
     */
    public SquigglyNode(ParseContext context, SquigglyName name, List<SquigglyNode> children, int stage, List<FunctionNode> keyFunctions, List<FunctionNode> valueFunctions, boolean negated, boolean squiggly, boolean emptyNested, boolean deep, Integer minDepth, Integer maxDepth) {
        this.context = notNull(context);
        this.name = name;
        this.negated = negated;
        this.children = Collections.unmodifiableList(children);
        this.stage = stage;
        this.keyFunctions = Collections.unmodifiableList(keyFunctions);
        this.valueFunctions = Collections.unmodifiableList(valueFunctions);
        this.squiggly = squiggly;
        this.emptyNested = emptyNested;
        this.deep = deep;
        this.minDepth = minDepth;
        this.maxDepth = maxDepth;
    }

    public static void main(String[] args) {
        System.out.println(Boolean.compare(true, false));
    }

    @Override
    public int compareTo(SquigglyNode other) {
//        int cmp;
//
//        if ((cmp = Integer.compare(stage, other.stage)) != 0) {
//            return cmp;
//        }
//
//        if ((cmp = Integer.compare(stage, other.stage)) != 0) {
//            return cmp;
//        }

        return Integer.compare(getSpecificity(), other.getSpecificity());
    }

    /**
     * Indicates how specific the name is.  The higher the value, the more specific.
     *
     * @return specificity
     */
    public int getSpecificity() {
        return name.getSpecificity();
    }

    /**
     * Determines if the supplied name matches the current name.
     *
     * @param name a name
     * @return true if matches, false otherwise
     */
    public boolean matches(String otherName) {
        return name.matches(otherName);
    }

    /**
     * Get context.
     *
     * @return parse context
     */
    public ParseContext getContext() {
        return context;
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
     * Get the node's children.
     *
     * @return child nodes
     */
    public List<SquigglyNode> getChildren() {
        return children;
    }

    /**
     * Gets the stage of the node.
     *
     * @return stage
     */
    public int getStage() {
        return stage;
    }

    /**
     * Get the key functions.
     *
     * @return key functions
     */
    public List<FunctionNode> getKeyFunctions() {
        return keyFunctions;
    }

    /**
     * Get the value functions.
     *
     * @return value functions.
     */
    public List<FunctionNode> getValueFunctions() {
        return valueFunctions;
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
     * Says whether this node is *
     *
     * @return true if *, false if not
     */
    public boolean isAnyShallow() {
        return AnyShallowName.ID.equals(name.getName());
    }

    /**
     * Says whether this node explicitly specified no children.  (eg. assignee{})
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

    /**
     * Says whether node is a variable reference.
     *
     * @return true if variable reference, false otherwise
     */
    public boolean isVariable() {
        return name instanceof VariableName;
    }

    /**
     * Says whether node is deep.
     *
     * @return true if deep, false otherwise
     */
    public boolean isDeep() {
        return deep;
    }

    /**
     * Gets the min depth of a node.
     *
     * @return min depth
     */
    public Integer getMinDepth() {
        return minDepth;
    }

    /**
     * Gets the max depth of node.
     *
     * @return max depth
     */
    public Integer getMaxDepth() {
        return maxDepth;
    }

    /**
     * Determines if the node is available at the supplied data
     *
     * @param depth the depth
     * @return available
     */
    public boolean isAvailableAtDepth(int depth) {
        if (!isDeep()) {
            return false;
        }

        if (minDepth != null && depth < minDepth) {
            return false;
        }

        if (maxDepth != null && depth >= maxDepth) {
            return false;
        }

        return true;
    }

    /**
     * Create a squiggly node with the specified name.
     *
     * @param newName new name
     * @return ndoe
     */
    public SquigglyNode withName(SquigglyName newName) {
        return new SquigglyNode(context, newName, children, stage, keyFunctions, valueFunctions, negated, squiggly, emptyNested, deep, minDepth, maxDepth);
    }

    /**
     * Create a squiggly with the specified children.
     *
     * @param newChildren new children
     * @return children
     */
    public SquigglyNode withChildren(List<SquigglyNode> newChildren) {
        return new SquigglyNode(context, name, newChildren, stage, keyFunctions, valueFunctions, negated, squiggly, emptyNested, deep, minDepth, maxDepth);
    }

    public static SquigglyNode createNamed(SquigglyName name) {
        return new SquigglyNode(new ParseContext(1, 1), name, Collections.emptyList(), 0, Collections.emptyList(), Collections.emptyList(), false, false, false, false, null, null);
    }

    public static SquigglyNode createNamedSquiggly(SquigglyName name) {
        return new SquigglyNode(new ParseContext(1, 1), name, Collections.emptyList(), 0, Collections.emptyList(), Collections.emptyList(), false, true, false, false, null, null);
    }
}

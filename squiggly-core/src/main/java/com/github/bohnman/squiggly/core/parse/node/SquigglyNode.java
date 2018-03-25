package com.github.bohnman.squiggly.core.parse.node;

import com.github.bohnman.squiggly.core.name.AnyDeepName;
import com.github.bohnman.squiggly.core.name.AnyShallowName;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.name.SquigglyName;
import com.github.bohnman.squiggly.core.name.VariableName;
import com.github.bohnman.squiggly.core.parse.ParseContext;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Collections;
import java.util.List;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode {

    public static final String ROOT = "root";
    public static final SquigglyNode EMPTY = new SquigglyNode(
            new ParseContext(1, 1),
            new ExactName(ROOT),
            Collections.emptyList(),
            Collections.emptyList(),
            Collections.emptyList(),
            false,
            false,
            false,
            false,
            null,
            null);

    private final ParseContext context;
    private final SquigglyName name;
    private final List<SquigglyNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final boolean squiggly;
    private final boolean negated;
    private final boolean emptyNested;
    private final boolean recursive;
    private final Integer startDepth;
    private final Integer endDepth;

    public SquigglyNode(ParseContext context, SquigglyName name, List<SquigglyNode> children, List<FunctionNode> keyFunctions, List<FunctionNode> valueFunctions, boolean squiggly, boolean negated, boolean emptyNested, boolean recursive, Integer startDepth, Integer endDepth) {
        this.context = context;
        this.name = name;
        this.children = children;
        this.keyFunctions = keyFunctions;
        this.valueFunctions = valueFunctions;
        this.squiggly = squiggly;
        this.negated = negated;
        this.emptyNested = emptyNested;
        this.recursive = recursive;
        this.startDepth = startDepth;
        this.endDepth = endDepth;
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


    public boolean isRecursive() {
        return recursive;
    }

    public Integer getStartDepth() {
        return startDepth;
    }

    public Integer getEndDepth() {
        return endDepth;
    }

    /**
     * Create a squiggly node with the specified name.
     *
     * @param newName new name
     * @return ndoe
     */
    public SquigglyNode withName(SquigglyName newName) {
        return new SquigglyNode(context, newName, children, keyFunctions, valueFunctions, squiggly, negated, emptyNested, recursive, startDepth, endDepth);
    }

    /**
     * Create a squiggly with the specified children.
     *
     * @param newChildren new children
     * @return children
     */
    public SquigglyNode withChildren(List<SquigglyNode> newChildren) {
        return new SquigglyNode(context, name, newChildren, keyFunctions, valueFunctions, squiggly, negated, emptyNested, recursive, startDepth, endDepth);
    }
}

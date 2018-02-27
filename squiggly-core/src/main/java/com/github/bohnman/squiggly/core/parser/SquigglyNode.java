package com.github.bohnman.squiggly.core.parser;

import com.github.bohnman.squiggly.core.name.AnyDeepName;
import com.github.bohnman.squiggly.core.name.AnyShallowName;
import com.github.bohnman.squiggly.core.name.SquigglyName;
import com.github.bohnman.squiggly.core.name.VariableName;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Collections;
import java.util.List;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode {

    private final ParseContext context;
    private final SquigglyName name;
    private final List<SquigglyNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final boolean squiggly;
    private final boolean negated;
    private final boolean emptyNested;

    /**
     * Constructor.
     *
     * @param context        parser context
     * @param name           name of the node
     * @param children       child nodes
     * @param keyFunctions   key functions
     * @param valueFunctions value functions
     * @param negated        whether or not the node has been negated
     * @param squiggly       whether or not a node is squiggly
     * @param emptyNested    whether of not filter specified {}
     * @see #isSquiggly()
     */
    public SquigglyNode(ParseContext context, SquigglyName name, List<SquigglyNode> children, List<FunctionNode> keyFunctions, List<FunctionNode> valueFunctions, boolean negated, boolean squiggly, boolean emptyNested) {
        this.context = notNull(context);
        this.name = name;
        this.negated = negated;
        this.children = Collections.unmodifiableList(children);
        this.keyFunctions = Collections.unmodifiableList(keyFunctions);
        this.valueFunctions = Collections.unmodifiableList(valueFunctions);
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

    /**
     * Create a squiggly node with the specified name.
     *
     * @param newName new name
     * @return ndoe
     */
    public SquigglyNode withName(SquigglyName newName) {
        return new SquigglyNode(context, newName, children, keyFunctions, valueFunctions, negated, squiggly, emptyNested);
    }
}

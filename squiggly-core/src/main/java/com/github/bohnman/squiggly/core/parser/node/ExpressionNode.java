package com.github.bohnman.squiggly.core.parser.node;

import com.github.bohnman.squiggly.core.name.*;
import com.github.bohnman.squiggly.core.parser.ParseContext;

import javax.annotation.concurrent.ThreadSafe;
import java.util.Collections;
import java.util.List;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class ExpressionNode extends BaseSquigglyNode implements Comparable<ExpressionNode> {

    private final SquigglyName name;
    private final int modifiers;
    private final List<ExpressionNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final Integer minDepth;
    private final Integer maxDepth;
    private final int stage;
    private final int depth;

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
     * @see #isNested()
     */
    public ExpressionNode(ParseContext context, SquigglyName name, int modifiers, List<ExpressionNode> children, int stage, int depth, List<FunctionNode> keyFunctions, List<FunctionNode> valueFunctions, Integer minDepth, Integer maxDepth) {
        super(context, SquigglyNodeType.EXPRESSION);
        this.name = name;
        this.modifiers = modifiers;
        this.children = Collections.unmodifiableList(children);
        this.depth = depth;
        this.stage = stage;
        this.keyFunctions = Collections.unmodifiableList(keyFunctions);
        this.valueFunctions = Collections.unmodifiableList(valueFunctions);
        this.minDepth = minDepth;
        this.maxDepth = maxDepth;
    }

    public static void main(String[] args) {
        System.out.println(Boolean.compare(true, false));
    }

    @Override
    public int compareTo(ExpressionNode other) {
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
    public List<ExpressionNode> getChildren() {
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

    public int getDepth() {
        return depth;
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
    public boolean isNested() {
        return Modifier.isNested(modifiers);
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
        return isNested() && children.isEmpty();
    }

    /**
     * Says whether the node started with '-'.
     *
     * @return true if negated false if not
     */
    public boolean isNegated() {
        return Modifier.isNegated(modifiers);
    }


    /**
     * Says whether node is deep.
     *
     * @return true if deep, false otherwise
     */
    public boolean isDeep() {
        return Modifier.isDeep(modifiers);
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
    public ExpressionNode withName(SquigglyName newName) {
        return new ExpressionNode(getContext(), newName, modifiers, children, stage, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
    }

    /**
     * Create a squiggly with the specified children.
     *
     * @param newChildren new children
     * @return children
     */
    public ExpressionNode withChildren(List<ExpressionNode> newChildren) {
        return new ExpressionNode(getContext(), name, modifiers, newChildren, stage, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
    }

    public static ExpressionNode createNamed(SquigglyName name) {
        return new ExpressionNode(new ParseContext(1, 1), name, 0, Collections.emptyList(), 0, 0, Collections.emptyList(), Collections.emptyList(), null, null);
    }

    public static ExpressionNode createNamedNested(SquigglyName name) {
        return new ExpressionNode(new ParseContext(1, 1), name, Modifier.NESTED, Collections.emptyList(), 0, 0, Collections.emptyList(), Collections.emptyList(), null, null);
    }


    public static class Modifier {

        private static final int DEEP = 0x00000001;
        private static final int NEGATED = 0x00000002;
        private static final int NESTED = 0x00000004;


        public static boolean isDeep(int mod) {
            return (mod & DEEP) != 0;
        }

        public static int setDeep(int mod, boolean flag) {
            return (flag) ? mod | DEEP : mod & ~DEEP;
        }

        public static boolean isNegated(int mod) {
            return (mod & NEGATED) != 0;
        }

        public static int setNegated(int mod, boolean flag) {
            return (flag) ? mod | NEGATED : mod & ~NEGATED;
        }

        public static boolean isNested(int mod) {
            return (mod & NESTED) != 0;
        }

        public static int setNested(int mod, boolean flag) {
            return (flag) ? mod | NESTED : mod & ~NESTED;
        }
    }
}

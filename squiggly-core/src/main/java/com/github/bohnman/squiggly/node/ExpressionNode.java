package com.github.bohnman.squiggly.node;

import com.github.bohnman.squiggly.name.*;
import com.github.bohnman.squiggly.parse.SquigglyParseException;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.util.*;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class ExpressionNode extends BaseNode implements Comparable<ExpressionNode> {

    private final SquigglyName name;
    private final int modifiers;
    private final List<ExpressionNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final Integer minDepth;
    private final Integer maxDepth;
    private final int depth;

    /**
     * Constructor.
     *
     * @param origin        parser origin
     * @param name           name of the node
     * @param children       child nodes
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
    private ExpressionNode(
            SquigglyNodeOrigin origin,
            SquigglyName name,
            int modifiers,
            List<ExpressionNode> children,
            int depth,
            List<FunctionNode> keyFunctions,
            List<FunctionNode> valueFunctions,
            Integer minDepth,
            Integer maxDepth) {
        super(origin);
        this.name = name;
        this.modifiers = modifiers;
        this.children = Collections.unmodifiableList(children);
        this.depth = depth;
        this.keyFunctions = Collections.unmodifiableList(keyFunctions);
        this.valueFunctions = Collections.unmodifiableList(valueFunctions);
        this.minDepth = minDepth;
        this.maxDepth = maxDepth;
    }

    @Override
    public int compareTo(ExpressionNode other) {
        int cmp = Integer.compare(depth, other.depth);

        if (cmp == 0) {
            int thisShallow = isDeep() ? 0 : 1;
            int otherShallow = other.isDeep() ? 0 : 1;
            cmp = Integer.compare(thisShallow, otherShallow);
        }

        if (cmp == 0) {
            cmp = Integer.compare(getSpecificity(), other.getSpecificity());
        }

        return cmp;
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
        return name.getToken();
    }

    /**
     * Get the node's children.
     *
     * @return child nodes
     */
    public List<ExpressionNode> getChildren() {
        return children;
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
        return SquigglyNames.ANY_DEEP_TOKEN.equals(name.getToken());
    }


    public boolean isDeepInherit() {
        return SquigglyNames.DEEP_INHERIT_TOKEN.equals(name.getToken());
    }

    /**
     * Says whether this node is *
     *
     * @return true if *, false if not
     */
    public boolean isAnyShallow() {
        return SquigglyNames.ANY_SHALLOW_TOKEN.equals(name.getToken());
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
     * Determines if the node is available at the supplied depth
     *
     * @param depth the depth
     * @return available
     */
    public boolean isAvailableAtDepth(int depth) {
        if (this.depth == depth) {
            return true;
        }

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
        return new ExpressionNode(getOrigin(), newName, modifiers, children, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
    }

    /**
     * Create a squiggly with the specified children.
     *
     * @param newChildren new children
     * @return children
     */
    public ExpressionNode withChildren(List<ExpressionNode> newChildren) {
        return new ExpressionNode(getOrigin(), name, modifiers, newChildren, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
    }

    public static ExpressionNode createNamed(SquigglyName name) {
        return new ExpressionNode(SquigglyNodeOrigin.create(), name, 0, Collections.emptyList(), 0, Collections.emptyList(), Collections.emptyList(), null, null);
    }

    public static ExpressionNode createNamedNested(SquigglyName name) {
        return new ExpressionNode(SquigglyNodeOrigin.create(), name, Modifier.NESTED, Collections.emptyList(), 0, Collections.emptyList(), Collections.emptyList(), null, null);
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

    public static class Builder extends BaseNodeBuilder<ExpressionNode> {
        private boolean negativeParent;

        @Nullable
        private SquigglyName name;
        private int modifiers;
        private int depth;
        @Nullable
        private Map<String, Builder> children;
        private boolean dotPathed;
        @Nullable
        private Builder parent;
        private List<FunctionNode> keyFunctions = new ArrayList<>();
        private List<FunctionNode> valueFunctions = new ArrayList<>();

        @Nullable
        private Integer minDepth;

        @Nullable
        private Integer maxDepth;

        Builder(SquigglyNodeOrigin origin) {
            super(origin);
        }

        public Map<String, Builder> getChildren() {
            if (children == null) {
                return Collections.emptyMap();
            }

            return children;
        }

        public int getDepth() {
            return depth;
        }

        public int getModifiers() {
            return modifiers;
        }

        public boolean isDotPathed() {
            return dotPathed;
        }

        public boolean isNegativeParent() {
            return negativeParent;
        }

        public ExpressionNode build() {
            if (name == null) {
                throw new SquigglyParseException(getOrigin(), "no name specified.");
            }

            List<ExpressionNode> childNodes;

            if (children == null || children.isEmpty()) {
                childNodes = Collections.emptyList();
            } else {
                childNodes = new ArrayList<>(children.size());

                for (Builder child : children.values()) {
                    childNodes.add(child.build());
                }
            }

            return new ExpressionNode(getOrigin(), name, modifiers, childNodes, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
        }

        public Builder depth(int depth) {
            this.depth = depth;
            return this;
        }

        public Builder dotPathed(boolean dotPathed) {
            this.dotPathed = dotPathed;
            return this;
        }

        @SuppressWarnings("UnusedReturnValue")
        public Builder keyFunctions(List<FunctionNode> functions) {
            functions.forEach(this::keyFunction);
            return this;
        }

        public Builder keyFunction(FunctionNode function) {
            keyFunctions.add(function);
            return this;
        }

        @SuppressWarnings("UnusedReturnValue")
        public Builder valueFunctions(List<FunctionNode> functions) {
            functions.forEach(this::valueFunction);
            return this;
        }

        public Builder valueFunction(FunctionNode function) {
            valueFunctions.add(function);
            return this;
        }

        public Builder name(SquigglyName name) {
            this.name = name;
            return this;
        }

        public Builder nested(boolean nested) {
            modifiers = Modifier.setNested(modifiers, nested);
            return this;
        }

        public Builder negated(boolean negated) {
            modifiers = Modifier.setNegated(modifiers, negated);
            return this;
        }

        public Builder deep(boolean deep) {
            modifiers = Modifier.setDeep(modifiers, deep);
            return this;
        }

        public Builder minDepth(Integer minDepth) {
            this.minDepth = minDepth;
            return this;
        }

        public Builder maxDepth(Integer maxDepth) {
            this.maxDepth = maxDepth;
            return this;
        }

        public Builder negativeParent(boolean negativeParent) {
            this.negativeParent = negativeParent;
            return null;
        }

        public Builder child(Builder childToAdd) {
            if (children == null) {
                children = new LinkedHashMap<>();
            }

            String name = childToAdd.name.getToken();
            Builder existingChild = children.get(name);

            if (existingChild == null) {
                childToAdd.parent = this;
                children.put(name, childToAdd);
            } else {
                if (childToAdd.children != null) {

                    if (existingChild.children == null) {
                        existingChild.children = childToAdd.children;
                    } else {
                        existingChild.children.putAll(childToAdd.children);
                    }
                }


                existingChild.nested(Modifier.isNested(existingChild.modifiers) || Modifier.isNested(childToAdd.modifiers));
                existingChild.dotPathed = existingChild.dotPathed && childToAdd.dotPathed;
                existingChild.negativeParent = existingChild.negativeParent && childToAdd.negativeParent;
                childToAdd = existingChild;
            }

            if (!childToAdd.dotPathed && dotPathed) {
                dotPathed = false;
            }

            childToAdd.depth(depth + 1);

            return childToAdd;
        }


    }
}

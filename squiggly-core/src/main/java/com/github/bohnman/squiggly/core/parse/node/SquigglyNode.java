package com.github.bohnman.squiggly.core.parse.node;

import com.github.bohnman.squiggly.core.name.AnyDeepName;
import com.github.bohnman.squiggly.core.name.AnyShallowName;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.name.SquigglyName;
import com.github.bohnman.squiggly.core.name.VariableName;
import com.github.bohnman.squiggly.core.parse.ParseContext;

import javax.annotation.concurrent.ThreadSafe;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A squiggly node represents a component of a filter expression.
 */
@ThreadSafe
public class SquigglyNode {

    public static final String ROOT = "root";
    public static final SquigglyNode EMPTY = builder().build();

    private final ParseContext context;
    private final SquigglyName name;
    private final int depth;
    private final List<SquigglyNode> children;
    private final List<FunctionNode> keyFunctions;
    private final List<FunctionNode> valueFunctions;
    private final boolean squiggly;
    private final boolean negated;
    private final boolean emptyNested;
    private final boolean recursive;
    private final Integer startDepth;
    private final Integer endDepth;

    public SquigglyNode(
            ParseContext context,
            SquigglyName name,
            int depth,
            List<SquigglyNode> children,
            List<FunctionNode> keyFunctions,
            List<FunctionNode> valueFunctions,
            boolean squiggly,
            boolean negated,
            boolean emptyNested,
            boolean recursive,
            Integer startDepth,
            Integer endDepth) {
        this.context = context;
        this.name = name;
        this.depth = depth;
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

    public int getDepth() {
        return depth;
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
     * @param name new name
     * @return ndoe
     */
    public SquigglyNode withName(SquigglyName name) {
        return toBuilder().name(name).build();
    }

    /**
     * Create a squiggly with the specified children.
     *
     * @param children new children
     * @return children
     */
    public SquigglyNode withChildren(List<SquigglyNode> children) {
        return toBuilder().children(children).build();
    }

    public Builder toBuilder() {
        return new Builder(this);
    }


    public static Builder builder() {
        return new Builder();
    }

    public static Builder builder(SquigglyNode node) {
        return new Builder(node);
    }

    public static class Builder<B extends SquigglyNode.Builder> {
        protected ParseContext context;
        protected SquigglyName name;
        protected int depth;
        protected List<SquigglyNode> children;
        private List<FunctionNode> keyFunctions;
        private List<FunctionNode> valueFunctions;
        protected boolean squiggly;
        protected boolean negated;
        protected boolean emptyNested;
        private boolean recursive;
        protected Integer startDepth;
        protected Integer endDepth;

        protected Builder() {
        }

        protected Builder(SquigglyNode node) {
            context = node.context;
            name = node.name;
            depth = node.depth;
            children = node.children;
            keyFunctions = new ArrayList<>(node.keyFunctions);
            valueFunctions = new ArrayList<>(node.valueFunctions);
            squiggly = node.squiggly;
            negated = node.negated;
            emptyNested = node.emptyNested;
            recursive = node.recursive;
            startDepth = node.startDepth;
            endDepth = node.endDepth;
        }

        public B context(ParseContext context) {
            this.context = context;
            return getThis();
        }

        public B name(SquigglyName name) {
            this.name = name;
            return getThis();
        }

        public B depth(int depth) {
            this.depth = depth;
            return getThis();
        }

        public B children(List<SquigglyNode> children) {
            this.children = children;
            return getThis();
        }

        @SuppressWarnings("UnusedReturnValue")
        public B keyFunctions(List<FunctionNode> functions) {
            functions.forEach(this::keyFunction);
            return getThis();
        }

        public B keyFunction(FunctionNode function) {
            if (keyFunctions == null) {
                keyFunctions = new ArrayList<>();
            }

            keyFunctions.add(function);
            return getThis();
        }

        @SuppressWarnings("UnusedReturnValue")
        public B valueFunctions(List<FunctionNode> functions) {
            functions.forEach(this::valueFunction);
            return getThis();
        }

        public B valueFunction(FunctionNode function) {
            if (valueFunctions == null) {
                valueFunctions = new ArrayList<>();
            }

            valueFunctions.add(function);
            return getThis();
        }

        public B squiggly(boolean squiggly) {
            this.squiggly = squiggly;
            return getThis();
        }

        public B negated(boolean negated) {
            this.negated = negated;
            return getThis();
        }

        public B emptyNested(boolean emptyNested) {
            this.emptyNested = emptyNested;
            return getThis();
        }

        public B recursive(boolean recursive) {
            this.recursive = recursive;
            return getThis();
        }

        public B startDepth(Integer startDepth) {
            this.startDepth = startDepth;
            return getThis();
        }

        public B endDepth(Integer endDepth) {
            this.endDepth = endDepth;
            return getThis();
        }

        @SuppressWarnings("unchecked")
        private B getThis() {
            return (B) this;
        }

        public SquigglyNode build() {
            if (context == null) {
                context = new ParseContext(1, 1);
            }

            if (name == null) {
                name = new ExactName(SquigglyNode.ROOT);
            }

            List<FunctionNode> keyFunctions = this.keyFunctions;

            if (keyFunctions == null) {
                keyFunctions = Collections.emptyList();
            } else {
                keyFunctions = Collections.unmodifiableList(this.keyFunctions);
            }

            List<FunctionNode> valueFunctions = this.valueFunctions;

            if (valueFunctions == null) {
                valueFunctions = Collections.emptyList();
            } else {
                valueFunctions = Collections.unmodifiableList(this.valueFunctions);
            }

            List<SquigglyNode> children = this.children;

            if (children == null) {
                children = Collections.emptyList();
            } else {
                children = Collections.unmodifiableList(this.children);
            }

            return new SquigglyNode(
                    context,
                    name,
                    depth,
                    children,
                    keyFunctions,
                    valueFunctions,
                    squiggly,
                    negated,
                    emptyNested,
                    recursive,
                    startDepth,
                    endDepth);
        }
    }

    @Override
    public String toString() {
        return getName();
    }
}

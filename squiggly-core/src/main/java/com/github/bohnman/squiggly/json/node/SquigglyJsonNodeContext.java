package com.github.bohnman.squiggly.json.node;

import com.github.bohnman.squiggly.path.SquigglyObjectPath;
import com.github.bohnman.squiggly.path.SquigglyObjectPathElement;

public class SquigglyJsonNodeContext<T> {

    private static final Object ROOT_KEY = new Object();

    private final int depth;
    private Object key;
    private final SquigglyObjectPath objectPath;
    private final SquigglyObjectPath absolutePath;
    private final SquigglyJsonNode<T> parentNode;

    public SquigglyJsonNodeContext() {
        this(0, ROOT_KEY, null, SquigglyObjectPath.empty(), SquigglyObjectPath.empty());
    }

    private SquigglyJsonNodeContext(int depth, Object key, SquigglyJsonNode<T> parentNode, SquigglyObjectPath absolutePath, SquigglyObjectPath objectPath) {
        this.depth = depth;
        this.key = key;
        this.absolutePath = absolutePath;
        this.objectPath = objectPath;
        this.parentNode = parentNode;
    }

    public int getDepth() {
        return depth;
    }

    public Object getKey() {
        return key;
    }

    public void setKey(Object key) {
        this.key = key;
    }

    public SquigglyObjectPath getObjectPath() {
        return objectPath;
    }

    public SquigglyJsonNode<T> getParentNode() {
        return parentNode;
    }

    public SquigglyJsonNodeContext<T> descend(Object key, SquigglyJsonNode<T> parent, SquigglyObjectPathElement absolutePathElement, SquigglyObjectPathElement objectPathElement) {
        SquigglyObjectPath absolutePath = (absolutePathElement == null) ? this.absolutePath : this.absolutePath.append(absolutePathElement);
        SquigglyObjectPath objectPath = (objectPathElement == null) ? this.objectPath : this.objectPath.append(objectPathElement);
        return new SquigglyJsonNodeContext<>(depth + 1, key, parent, absolutePath, objectPath);
    }
}

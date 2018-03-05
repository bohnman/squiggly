package com.github.bohnman.squiggly.jackson.json.node;

import com.github.bohnman.squiggly.jackson.json.path.CoreJsonPath;
import com.github.bohnman.squiggly.jackson.json.path.CoreJsonPathElement;

public class CoreJsonNodeVisitorContext {

    private static final Object ROOT_KEY = new Object();


    private final int depth;
    private Object key;
    private final CoreJsonPath path;

    public CoreJsonNodeVisitorContext() {
        this(0, ROOT_KEY, CoreJsonPath.empty());
    }

    public CoreJsonNodeVisitorContext(int depth, Object key, CoreJsonPath path) {
        this.depth = depth;
        this.key = key;
        this.path = path;
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

    public CoreJsonPath getPath() {
        return path;
    }

    public CoreJsonNodeVisitorContext descend(Object key) {
        return new CoreJsonNodeVisitorContext(depth + 1, key, path);
    }

    public CoreJsonNodeVisitorContext descend(Object key, CoreJsonPathElement pathElement) {
        return new CoreJsonNodeVisitorContext(depth + 1, key, path.add(pathElement));
    }
}

package com.github.bohnman.squiggly.json;

import com.github.bohnman.squiggly.json.filter.SquigglyJsonNodeFilter;
import com.github.bohnman.squiggly.json.node.SquigglyJsonNode;
import com.github.bohnman.squiggly.runtime.SquigglyRuntime;

public abstract class SquigglyJson {

    private final SquigglyJsonNodeFilter nodeFilter;
    private final SquigglyRuntime runtime;

    public SquigglyJson(SquigglyRuntime runtime) {
        this.runtime = runtime;
        this.nodeFilter = createNodeFilter(runtime);
    }

    public SquigglyRuntime getRuntime() {
        return runtime;
    }

    protected SquigglyJsonNodeFilter createNodeFilter(SquigglyRuntime runtime) {
        return new SquigglyJsonNodeFilter(
                runtime.getConfig(),
                runtime.getExpressionMatcher(),
                runtime.getContextProvider(),
                runtime.getFilterRepository(),
                runtime.getFunctionInvoker(),
                runtime.getParser());
    }

    public <T> SquigglyJsonNode<T> filter(SquigglyJsonNode<T> node, String... filters) {
        return nodeFilter.apply(node, filters);
    }
}

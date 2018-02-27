package com.github.bohnman.squiggly.core.context;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;
import net.jcip.annotations.NotThreadSafe;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.util.stream.Collectors.toList;

/**
 * Squiggly context that loads the parsed nodes on demand.
 */
@NotThreadSafe
public class LazySquigglyContext implements SquigglyContext {

    private final Class beanClass;
    private final String filter;
    private final BaseSquiggly squiggly;

    @Nullable
    private List<SquigglyNode> nodes;

    public LazySquigglyContext(Class beanClass, BaseSquiggly squiggly, String filter) {
        this.beanClass = notNull(beanClass);
        this.squiggly = notNull(squiggly);
        notNull(filter);
        this.filter = notNull(CoreObjects.firstNonNull(squiggly.getFilterRepository().findByName(filter), filter));
    }

    @Override
    public Class getBeanClass() {
        return beanClass;
    }

    @Override
    public List<SquigglyNode> getNodes() {
        if (nodes == null) {
            nodes = normalize(squiggly.getParser().parse(filter));
        }

        return nodes;
    }

    private List<SquigglyNode> normalize(List<SquigglyNode> nodes) {
        return nodes.stream()
                .map(node -> {
                    if (node.isVariable()) {
                        SquigglyVariableResolver variableResolver = squiggly.getVariableResolver();
                        String value = Objects.toString(variableResolver.resolveVariable(node.getName()));

                        if (value == null) {
                            value = ':' + node.getName();
                        }

                        node = node.withName(new ExactName(value));
                    }

                    return node;
                })
                .collect(toList());
    }

    @Override
    public String getFilter() {
        return filter;
    }
}

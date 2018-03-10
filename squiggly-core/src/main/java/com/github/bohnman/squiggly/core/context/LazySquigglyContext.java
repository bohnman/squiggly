package com.github.bohnman.squiggly.core.context;

import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.parser.SquigglyNode;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import javax.annotation.concurrent.NotThreadSafe;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Squiggly context that loads the parsed nodes on demand.
 */
@NotThreadSafe
public class LazySquigglyContext implements SquigglyContext {

    private final Class beanClass;
    private final String filter;
    private final BaseSquiggly squiggly;

    @Nullable
    private SquigglyNode node;

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
    public SquigglyNode getNode() {
        if (node == null) {
            node = squiggly.getNodeNormalizer().normalize(squiggly.getParser().parsePropertyFilter(filter));
        }

        return node;
    }

    @Override
    public String getFilter() {
        return filter;
    }
}

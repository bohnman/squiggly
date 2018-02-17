package com.github.bohnman.squiggly.context;

import com.github.bohnman.squiggly.filter.repository.SquigglyFilterRepository;
import com.github.bohnman.squiggly.parser.SquigglyNode;
import com.github.bohnman.squiggly.parser.SquigglyParser;
import com.google.common.base.MoreObjects;
import net.jcip.annotations.NotThreadSafe;

import javax.annotation.Nullable;
import java.util.List;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Squiggly context that loads the parsed nodes on demand.
 */
@NotThreadSafe
public class LazySquigglyContext implements SquigglyContext {

    private final Class beanClass;
    private final String filter;
    private final SquigglyFilterRepository filterRepository;

    @Nullable
    private List<SquigglyNode> nodes;
    private final SquigglyParser parser;

    public LazySquigglyContext(Class beanClass, SquigglyFilterRepository filterRepository, SquigglyParser parser, String filter) {
        this.beanClass = checkNotNull(beanClass);
        this.filterRepository = checkNotNull(filterRepository);
        this.parser = checkNotNull(parser);
        checkNotNull(filter);
        this.filter = checkNotNull(MoreObjects.firstNonNull(filterRepository.findByName(filter), filter));
    }

    @Override
    public Class getBeanClass() {
        return beanClass;
    }

    @Override
    public List<SquigglyNode> getNodes() {
        if (nodes == null) {
            nodes = parser.parse(filter);
        }

        return nodes;
    }

    @Override
    public String getFilter() {
        return filter;
    }
}

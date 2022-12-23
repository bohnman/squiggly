package dev.nicklasw.squiggly.context;

import dev.nicklasw.squiggly.parser.SquigglyNode;
import dev.nicklasw.squiggly.parser.SquigglyParser;
import net.jcip.annotations.NotThreadSafe;

import java.util.List;

/**
 * Squiggly context that loads the parsed nodes on demand.
 */
@NotThreadSafe
public class LazySquigglyContext implements SquigglyContext {

    private final Class beanClass;
    private final String filter;
    private List<SquigglyNode> nodes;
    private final SquigglyParser parser;

    public LazySquigglyContext(Class beanClass, SquigglyParser parser, String filter) {
        this.beanClass = beanClass;
        this.parser = parser;
        this.filter = filter;
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

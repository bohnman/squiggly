package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.github.bohnman.squiggly.bean.BeanInfo;
import com.github.bohnman.squiggly.bean.BeanInfoIntrospector;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.metric.source.GuavaCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.metric.source.SquigglyMetricsSource;
import com.github.bohnman.squiggly.name.AnyDeepName;
import com.github.bohnman.squiggly.name.ExactName;
import com.github.bohnman.squiggly.parser.SquigglyNode;
import com.github.bohnman.squiggly.view.PropertyView;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.Sets;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.tuple.Pair;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * A Jackson @{@link com.fasterxml.jackson.databind.ser.PropertyFilter} that filters objects using squiggly syntax.
 * <p>Here are some examples of squiggly syntax:</p>
 * <pre>
 *    // grab the id and name fields
 *    id,name
 *
 *    // grab the id and nested first name and last name from a the user property
 *    id,user{firstName,lastName}
 *
 *    // grab the full object graph
 *    **
 *
 *    // grab just the base fields
 *    base
 *
 *    // grab all fields of the current object, but just the base fields of nested objects
 *    *
 *
 *    // grab fields starting with eco
 *    eco*
 *
 *    // grab fields ending with Time
 *    *Time
 *
 *    // grab fields containing Weight
 *    *Weight*
 *
 *    // grab the firstName field of the nested employee and manager objects
 *    employee{firstName},manager{firstName}
 *    employee|manager{firstName}
 *
 *    // grab all fields annotated with @PropertyView("hardware") or a derived annotation
 *    hardware
 * </pre>
 */
@ThreadSafe
public class SquigglyPropertyFilter extends SimpleBeanPropertyFilter {

    public static final String FILTER_ID = "squigglyFilter";

    /**
     * Cache that stores previous evalulated matches.
     */
    private static final Cache<Pair<Path, String>, Boolean> MATCH_CACHE;
    private static final SquigglyMetricsSource METRICS_SOURCE;
    private static final List<SquigglyNode> BASE_VIEW_NODES = Collections.singletonList(new SquigglyNode(new ExactName(PropertyView.BASE_VIEW), Collections.<SquigglyNode>emptyList(), false, true, false));

    static {
        MATCH_CACHE = CacheBuilder.from(SquigglyConfig.getFilterPathCacheSpec()).build();
        METRICS_SOURCE = new GuavaCacheSquigglyMetricsSource("squiggly.filter.pathCache.", MATCH_CACHE);
    }

    private final BeanInfoIntrospector beanInfoIntrospector;
    private final SquigglyContextProvider contextProvider;

    /**
     * Construct with a specified context provider.
     *
     * @param contextProvider context provider
     */
    public SquigglyPropertyFilter(SquigglyContextProvider contextProvider) {
        this(contextProvider, new BeanInfoIntrospector());
    }

    /**
     * Construct with a context provider and an introspector
     *
     * @param contextProvider          context provider
     * @param beanInfoIntrospector introspector
     */
    public SquigglyPropertyFilter(SquigglyContextProvider contextProvider, BeanInfoIntrospector beanInfoIntrospector) {
        this.contextProvider = contextProvider;
        this.beanInfoIntrospector = beanInfoIntrospector;
    }

    // create a path structure representing the object graph
    private Path getPath(PropertyWriter writer, JsonStreamContext sc) {
        LinkedList<PathElement> elements = new LinkedList<>();

        if (sc != null) {
            elements.add(new PathElement(writer.getName(), sc.getCurrentValue()));
            sc = sc.getParent();
        }

        while (sc != null) {
            if (sc.getCurrentName() != null && sc.getCurrentValue() != null) {
                elements.addFirst(new PathElement(sc.getCurrentName(), sc.getCurrentValue()));
            }
            sc = sc.getParent();
        }

        return new Path(elements);
    }

    private JsonStreamContext getStreamContext(JsonGenerator jgen) {
        return jgen.getOutputContext();
    }

    @Override
    protected boolean include(final BeanPropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    protected boolean include(final PropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }

    protected boolean include(final PropertyWriter writer, final JsonGenerator jgen) {
        if (!contextProvider.isFilteringEnabled()) {
            return true;
        }

        JsonStreamContext streamContext = getStreamContext(jgen);

        if (streamContext == null) {
            return true;
        }

        Path path = getPath(writer, streamContext);
        SquigglyContext context = contextProvider.getContext(path.getFirst().getBeanClass());
        String filter = context.getFilter();


        if (AnyDeepName.ID.equals(filter)) {
            return true;
        }

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            Pair<Path, String> pair = Pair.of(path, filter);
            Boolean match = MATCH_CACHE.getIfPresent(pair);

            if (match == null) {
                match = pathMatches(path, context);
            }

            MATCH_CACHE.put(pair, match);
            return match;
        }

        return pathMatches(path, context);
    }

    // perform the actual matching
    private boolean pathMatches(Path path, SquigglyContext context) {
        List<SquigglyNode> nodes = context.getNodes();
        Set<String> viewStack = null;
        SquigglyNode viewNode = null;

        int pathSize = path.getElements().size();
        int lastIdx = pathSize - 1;

        for (int i = 0; i < pathSize; i++) {
            PathElement element = path.getElements().get(i);

            if (viewNode != null && !viewNode.isSquiggly()) {
                Class beanClass = element.getBeanClass();

                if (beanClass != null && !Map.class.isAssignableFrom(beanClass)) {
                    Set<String> propertyNames = getPropertyNamesFromViewStack(element, viewStack);

                    if (!propertyNames.contains(element.getName())) {
                        return false;
                    }
                }

            } else if (nodes.isEmpty()) {
                return false;
            } else {

                SquigglyNode match = findBestSimpleNode(element, nodes);

                if (match == null) {
                    match = findBestViewNode(element, nodes);

                    if (match != null) {
                        viewNode = match;
                        viewStack = addToViewStack(viewStack, viewNode);
                    }
                } else if (match.isAnyShallow()) {
                    viewNode = match;
                } else if (match.isAnyDeep()) {
                    return true;
                }

                if (match == null) {
                    if (isJsonUnwrapped(element)) {
                        continue;
                    }

                    return false;
                }

                if (match.isNegated()) {
                    return false;
                }

                nodes = match.getChildren();

                if (i < lastIdx && nodes.isEmpty() && !match.isEmptyNested() && SquigglyConfig.isFilterImplicitlyIncludeBaseFields()) {
                    nodes = BASE_VIEW_NODES;
                }
            }
        }

        return true;
    }

    private boolean isJsonUnwrapped(PathElement element) {
        BeanInfo info = beanInfoIntrospector.introspect(element.getBeanClass());
        return info.isUnwrapped(element.getName());
    }

    private Set<String> getPropertyNamesFromViewStack(PathElement element, Set<String> viewStack) {
        if (viewStack == null) {
            return getPropertyNames(element, PropertyView.BASE_VIEW);
        }

        Set<String> propertyNames = Sets.newHashSet();

        for (String viewName : viewStack) {
            Set<String> names = getPropertyNames(element, viewName);

            if (names.isEmpty() && SquigglyConfig.isFilterImplicitlyIncludeBaseFields()) {
                names = getPropertyNames(element, PropertyView.BASE_VIEW);
            }

            propertyNames.addAll(names);
        }

        return propertyNames;
    }

    private SquigglyNode findBestViewNode(PathElement element, List<SquigglyNode> nodes) {
        if (Map.class.isAssignableFrom(element.getBeanClass())) {
            for (SquigglyNode node : nodes) {
                if (PropertyView.BASE_VIEW.equals(node.getName())) {
                    return node;
                }
            }
        } else {
            for (SquigglyNode node : nodes) {
                // handle view
                Set<String> propertyNames = getPropertyNames(element, node.getName());

                if (propertyNames.contains(element.getName())) {
                    return node;
                }
            }
        }

        return null;
    }

    private SquigglyNode findBestSimpleNode(PathElement element, List<SquigglyNode> nodes) {
        SquigglyNode match = null;
        int lastMatchStrength = -1;

        for (SquigglyNode node : nodes) {
            int matchStrength = node.match(element.getName());

            if (matchStrength < 0) {
                continue;
            }

            if (lastMatchStrength < 0 || matchStrength >= lastMatchStrength) {
                match = node;
                lastMatchStrength = matchStrength;
            }

        }

        return match;
    }

    private Set<String> addToViewStack(Set<String> viewStack, SquigglyNode viewNode) {
        if (!SquigglyConfig.isFilterPropagateViewToNestedFilters()) {
            return null;
        }

        if (viewStack == null) {
            viewStack = Sets.newHashSet();
        }

        viewStack.add(viewNode.getName());

        return viewStack;
    }

    private Set<String> getPropertyNames(PathElement element, String viewName) {
        Class beanClass = element.getBeanClass();

        if (beanClass == null) {
            return Collections.emptySet();
        }

        return beanInfoIntrospector.introspect(beanClass).getPropertyNamesForView(viewName);
    }

    @Override
    public void serializeAsField(final Object pojo, final JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {
        if (include(writer, jgen)) {
            contextProvider.serializeAsIncludedField(pojo, jgen, provider, writer);
        } else if (!jgen.canOmitFields()) {
            contextProvider.serializeAsExcludedField(pojo, jgen, provider, writer);
        }
    }

    public static SquigglyMetricsSource getMetricsSource() {
        return METRICS_SOURCE;
    }

    /*
            Represents the path structuore in the object graph
         */
    private static class Path {

        private final String id;
        private final LinkedList<PathElement> elements;

        public Path(LinkedList<PathElement> elements) {
            StringBuilder idBuilder = new StringBuilder();

            for (int i = 0; i < elements.size(); i++) {
                PathElement element = elements.get(i);

                if (i > 0) {
                    idBuilder.append('.');
                }

                idBuilder.append(element.getName());
            }

            id = idBuilder.toString();
            this.elements = elements;
        }

        public String getId() {
            return id;
        }

        public List<PathElement> getElements() {
            return elements;
        }

        public PathElement getFirst() {
            return elements.getFirst();
        }

        public PathElement getLast() {
            return elements.getLast();
        }

        // we use the last element because that is where the json stream context started
        public Class getBeanClass() {
            return getLast().getBeanClass();
        }

        // maps aren't cachable
        public boolean isCachable() {
            Class beanClass = getBeanClass();
            return beanClass != null && !Map.class.isAssignableFrom(beanClass);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Path path = (Path) o;
            Class beanClass = getBeanClass();
            Class oBeanClass = path.getBeanClass();

            if (!id.equals(path.id)) return false;
            if (beanClass != null ? !beanClass.equals(oBeanClass) : oBeanClass != null) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = id.hashCode();
            Class beanClass = getBeanClass();
            result = 31 * result + (beanClass != null ? beanClass.hashCode() : 0);
            return result;
        }
    }

    // represent a specific point in the path.
    private static class PathElement {
        private final String name;
        private final Class bean;

        public PathElement(String name, Object bean) {
            this.name = name;
            this.bean = bean.getClass();
        }

        public String getName() {
            return name;
        }

        public Class getBeanClass() {
            return bean;
        }
    }
}

package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.parser.SquigglyNode;
import com.github.bohnman.squiggly.view.PropertyView;
import com.github.bohnman.squiggly.view.PropertyViewIntrospector;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.Sets;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toSet;


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
     *
     */
    private static final Cache<Pair<Path, String>, Boolean> MATCH_CACHE;

    static {
        int maxSize = SquigglyConfig.getFilterPathCacheMaxSize();
        MATCH_CACHE = CacheBuilder.newBuilder().maximumSize(maxSize).build();
    }

    private final PropertyViewIntrospector propertyViewIntrospector;
    private final SquigglyContextProvider contextProvider;

    /**
     * Construct with a specified context provider.
     *
     * @param contextProvider context provider
     */
    public SquigglyPropertyFilter(SquigglyContextProvider contextProvider) {
        this(contextProvider, new PropertyViewIntrospector());
    }

    /**
     * Construct with a context provider and an introspector
     *
     * @param contextProvider          context provider
     * @param propertyViewIntrospector introspector
     */
    public SquigglyPropertyFilter(SquigglyContextProvider contextProvider, PropertyViewIntrospector propertyViewIntrospector) {
        this.contextProvider = contextProvider;
        this.propertyViewIntrospector = propertyViewIntrospector;
    }

    // create a path structure representing the object graph
    private Path getPath(final PropertyWriter writer, final JsonGenerator jgen) {
        LinkedList<PathElement> elements = new LinkedList<>();
        JsonStreamContext sc = jgen.getOutputContext();

        if (sc != null) {
            elements.add(new PathElement(writer.getName(), sc.getCurrentValue()));
            sc = sc.getParent();
        }

        while (sc != null) {
            if (sc.getCurrentName() != null) {
                elements.addFirst(new PathElement(sc.getCurrentName(), sc.getCurrentValue()));
            }
            sc = sc.getParent();
        }

        return new Path(elements);
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
        Path path = getPath(writer, jgen);
        SquigglyContext context = contextProvider.getContext();

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            Pair<Path, String> pair = Pair.of(path, context.getFilter());
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
        boolean parentIsView = false;
        Set<String> viewStack = null;


        for (PathElement element : path.getElements()) {
            SquigglyNode match = null;

            if (parentIsView) {

                Class beanClass = element.getBeanClass();

                if (beanClass != null && !Map.class.isAssignableFrom(beanClass)) {
                    Set<String> propertyNames;

                    if (viewStack == null) {
                        propertyNames = getPropertyNames(element, PropertyView.BASE_VIEW);
                    } else {
                        propertyNames = viewStack.stream()
                                .flatMap(viewName -> {
                                    Set<String> names = getPropertyNames(element, viewName);

                                    if (names.isEmpty() && SquigglyConfig.isFilterImplicitlyIncludeBaseFields()) {
                                        names = getPropertyNames(element, PropertyView.BASE_VIEW);
                                    }

                                    return names.stream();
                                })
                                .collect(toSet());
                    }

                    if (!propertyNames.contains(element.getName())) {
                        return false;
                    }
                }

            } else {

                SquigglyNode viewNode = null;

                for (SquigglyNode node : nodes) {

                    // handle **
                    if (node.isAnyDeep()) {
                        return true;
                    }

                    // handle *
                    if (node.isAnyShallow() && !node.isSquiggly()) {
                        viewNode = node;
                        break;
                    }

                    // handle exact match
                    if (node.nameMatches(element.getName())) {
                        viewNode = null;
                        match = node;
                        break;
                    }

                    // handle view
                    Set<String> propertyNames = getPropertyNames(element, node.getName());

                    if (propertyNames.contains(element.getName())) {
                        viewNode = node;
                    }
                }

                if (viewNode != null) {
                    parentIsView = true;
                    match = viewNode;
                    //noinspection ConstantConditions
                    viewStack = addToViewStack(viewStack, viewNode);

                }

                if (match == null) {
                    return false;
                }

                if (match.getChildren().isEmpty() && !match.isSquiggly()) {
                    parentIsView = true;
                    viewStack = addToViewStack(viewStack, viewNode);
                }

                nodes = match.getChildren();
            }
        }

        return true;
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

        return propertyViewIntrospector.getPropertyNames(beanClass, viewName);
    }

    @Override
    public void serializeAsField(final Object pojo, final JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {
        if (include(writer, jgen)) {
            writer.serializeAsField(pojo, jgen, provider);
        } else if (!jgen.canOmitFields()) {
            writer.serializeAsOmittedField(pojo, jgen, provider);
        }
    }

    /*
        Represents the path structuore in the object graph
     */
    private static class Path {

        private final String id;
        private final LinkedList<PathElement> elements;

        public Path(LinkedList<PathElement> elements) {
            id = StringUtils.join(elements.stream().map(PathElement::getName).collect(Collectors.toList()), '.');
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

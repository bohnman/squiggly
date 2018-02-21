package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.bean.BeanInfo;
import com.github.bohnman.squiggly.context.SquigglyContext;
import com.github.bohnman.squiggly.function.FunctionRequest;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyParameter;
import com.github.bohnman.squiggly.metric.source.GuavaCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.name.AnyDeepName;
import com.github.bohnman.squiggly.name.ExactName;
import com.github.bohnman.squiggly.parser.FunctionNode;
import com.github.bohnman.squiggly.parser.ParameterNode;
import com.github.bohnman.squiggly.parser.ParseContext;
import com.github.bohnman.squiggly.parser.SquigglyNode;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import com.github.bohnman.squiggly.view.PropertyView;
import com.google.common.base.MoreObjects;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.ObjectArrays;
import com.google.common.collect.Sets;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;
import static java.lang.String.format;


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
    private static final SquigglyNode NEVER_MATCH = new SquigglyNode(new ParseContext(1, 1), AnyDeepName.get(), Collections.emptyList(), Collections.emptyList(), false, false, false);
    private static final SquigglyNode ALWAYS_MATCH = new SquigglyNode(new ParseContext(1, 1), AnyDeepName.get(), Collections.emptyList(), Collections.emptyList(), false, false, false);

    /**
     * Cache that stores previous evaluated matches.
     */
    private final Cache<Pair<Path, String>, SquigglyNode> matchCache;
    private final List<SquigglyNode> baseViewNodes = Collections.singletonList(new SquigglyNode(new ParseContext(1, 1), new ExactName(PropertyView.BASE_VIEW), Collections.emptyList(), Collections.emptyList(), false, true, false));
    private final Squiggly squiggly;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyPropertyFilter(Squiggly squiggly) {
        this.squiggly = checkNotNull(squiggly);
        this.matchCache = CacheBuilder.from(squiggly.getConfig().getFilterPathCacheSpec()).build();
        squiggly.getMetrics().add(new GuavaCacheSquigglyMetricsSource("squiggly.filter.pathCache.", matchCache));
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

    private SquigglyNode match(final PropertyWriter writer, final JsonGenerator jgen) {
        if (!squiggly.getContextProvider().isFilteringEnabled()) {
            return ALWAYS_MATCH;
        }

        JsonStreamContext streamContext = getStreamContext(jgen);

        if (streamContext == null) {
            return ALWAYS_MATCH;
        }

        Path path = getPath(writer, streamContext);
        SquigglyContext context = squiggly.getContextProvider().getContext(path.getFirst().getBeanClass(), squiggly);
        String filter = context.getFilter();


        if (AnyDeepName.ID.equals(filter)) {
            return ALWAYS_MATCH;
        }

        if (path.isCachable()) {
            // cache the match result using the path and filter expression
            Pair<Path, String> pair = Pair.of(path, filter);
            SquigglyNode match = matchCache.getIfPresent(pair);

            if (match == null) {
                match = matchPath(path, context);
            }

            matchCache.put(pair, match);
            return match;
        }

        return matchPath(path, context);
    }

    // perform the actual matching
    private SquigglyNode matchPath(Path path, SquigglyContext context) {
        List<SquigglyNode> nodes = context.getNodes();
        Set<String> viewStack = null;
        SquigglyNode viewNode = null;
        SquigglyNode match = null;

        int pathSize = path.getElements().size();
        int lastIdx = pathSize - 1;

        for (int i = 0; i < pathSize; i++) {
            PathElement element = path.getElements().get(i);

            if (viewNode != null && !viewNode.isSquiggly()) {
                Class beanClass = element.getBeanClass();

                if (beanClass != null && !Map.class.isAssignableFrom(beanClass)) {
                    Set<String> propertyNames = getPropertyNamesFromViewStack(element, viewStack);

                    if (!propertyNames.contains(element.getName())) {
                        return NEVER_MATCH;
                    }
                }

            } else if (nodes.isEmpty()) {
                return NEVER_MATCH;
            } else {
                match = findBestSimpleNode(element, nodes);

                if (match == null) {
                    match = findBestViewNode(element, nodes);

                    if (match != null) {
                        viewNode = match;
                        viewStack = addToViewStack(viewStack, viewNode);
                    }
                } else if (match.isAnyShallow()) {
                    viewNode = match;
                } else if (match.isAnyDeep()) {
                    return match;
                }

                if (match == null) {
                    if (isJsonUnwrapped(element)) {
                        match = ALWAYS_MATCH;
                        continue;
                    }

                    return NEVER_MATCH;
                }

                if (match.isNegated()) {
                    return NEVER_MATCH;
                }

                nodes = match.getChildren();

                if (i < lastIdx && nodes.isEmpty() && !match.isEmptyNested() && squiggly.getConfig().isFilterImplicitlyIncludeBaseFields()) {
                    nodes = baseViewNodes;
                }
            }
        }

        if (match == null) {
            match = NEVER_MATCH;
        }

        return match;
    }


    private boolean isJsonUnwrapped(PathElement element) {
        BeanInfo info = squiggly.getBeanInfoIntrospector().introspect(element.getBeanClass());
        return info.isUnwrapped(element.getName());
    }

    private Set<String> getPropertyNamesFromViewStack(PathElement element, Set<String> viewStack) {
        if (viewStack == null) {
            return getPropertyNames(element, PropertyView.BASE_VIEW);
        }

        Set<String> propertyNames = Sets.newHashSet();

        for (String viewName : viewStack) {
            Set<String> names = getPropertyNames(element, viewName);

            if (names.isEmpty() && squiggly.getConfig().isFilterImplicitlyIncludeBaseFields()) {
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
        if (!squiggly.getConfig().isFilterPropagateViewToNestedFilters()) {
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

        return squiggly.getBeanInfoIntrospector().introspect(beanClass).getPropertyNamesForView(viewName);
    }

    @Override
    public void serializeAsField(final Object pojo, final JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {
        SquigglyNode match = match(writer, jgen);

        if (match != null && match != NEVER_MATCH) {
            if (match.getValueFunctions().isEmpty() || !(writer instanceof BeanPropertyWriter)) {
                squiggly.getSerializer().serializeAsIncludedField(pojo, jgen, provider, writer);
            } else {
                BeanPropertyWriter beanPropertyWriter = (BeanPropertyWriter) writer;
                Object value = executionFunctions(match, beanPropertyWriter.get(pojo));
                squiggly.getSerializer().serializeAsConvertedField(pojo, jgen, provider, writer, value);
            }


        } else if (!jgen.canOmitFields()) {
            squiggly.getSerializer().serializeAsExcludedField(pojo, jgen, provider, writer);
        }
    }

    private Object executionFunctions(SquigglyNode node, Object value) {
        for (FunctionNode functionNode : node.getValueFunctions()) {
            value = executeFunction(functionNode, value);
        }

        return value;
    }

    private Object executeFunction(FunctionNode functionNode, Object input) {
        List<SquigglyFunction<Object>> functions = squiggly.getFunctionRepository().findByName(functionNode.getName());

        if (functions.isEmpty()) {
            throw new IllegalStateException(format("%s: Unrecognized function [%s]", functionNode.getContext(), functionNode.getName()));
        }

        List<Object> requestedParameters = toParameters(functionNode, input);
        SquigglyFunction<Object> winner = findBestCandidateFunction(functionNode, requestedParameters, functions);

        if (winner == null) {
            throw new IllegalStateException(format("%s: Unable to match function [%s] with parameters %s.", functionNode.getContext(), functionNode.getName(), functionNode.getParameters()));
        }

        List<Object> parameters = convert(requestedParameters, winner);

        return winner.apply(new FunctionRequest(input, parameters));
    }

    private List<Object> convert(List<Object> requestedParameters, SquigglyFunction<Object> winner) {
        List<SquigglyParameter> configuredParameters = winner.getParameters();

        if (configuredParameters.isEmpty()) {
            return Collections.emptyList();
        }

        int requestedParametersSize = requestedParameters.size();
        int configuredParametersSize = configuredParameters.size();
        int varargsIndex = configuredParameters.get(configuredParametersSize - 1).isVarArgs() ? configuredParametersSize - 1 : -1;
        int end = (varargsIndex < 0) ? requestedParametersSize : Math.min(varargsIndex, requestedParametersSize);


        List<Object> parameters = new ArrayList<>();


        for (int i = 0; i < end; i++) {
            Object requestedParam = requestedParameters.get(i);
            SquigglyParameter configuredParam = configuredParameters.get(i);
            parameters.add(convert(requestedParam, configuredParam.getType()));
        }

        if (varargsIndex >= 0) {
            SquigglyParameter varargParameter = configuredParameters.get(varargsIndex);
            Class<?> varargType = MoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());
            int len = Math.max(0, requestedParametersSize - varargsIndex);
            Object[] array = ObjectArrays.newArray(varargType, len);

            for (int i = varargsIndex; i < requestedParametersSize; i++) {
                array[i - varargsIndex] = convert(requestedParameters.get(i), varargType);
            }

            parameters.add(array);
        }

        return Collections.unmodifiableList(parameters);
    }

    private Object convert(Object requestedParam, Class<?> type) {
        if (requestedParam == null) {
            return null;
        }

        if (type.equals(requestedParam.getClass())) {
            return requestedParam;
        }

        return squiggly.getConversionService().convert(requestedParam, type);
    }

    private List<Object> toParameters(FunctionNode functionNode, Object input) {
        return functionNode.getParameters()
                .stream()
                .map(parameterNode -> toParameter(functionNode, input, parameterNode))
                .collect(Collectors.toList());
    }

    private Object toParameter(FunctionNode functionNode, Object input, ParameterNode parameterNode) {
        switch (parameterNode.getType()) {
            case BOOLEAN:
                return parameterNode.getValue();
            case FLOAT:
                return parameterNode.getValue();
            case INPUT:
                return input;
            case INTEGER:
                return parameterNode.getValue();
            case REGEX:
                return parameterNode.getValue();
            case STRING:
                return parameterNode.getValue();
            case VARIABLE:
                return squiggly.getVariableResolver().resolveVariable(parameterNode.getValue().toString());
            default:
                throw new IllegalStateException(format("%s: Unhanded parameter node type [%s]", parameterNode.getContext(), parameterNode.getType()));
        }
    }

    private SquigglyFunction<Object> findBestCandidateFunction(FunctionNode functionNode, List<Object> requestedParameters, List<SquigglyFunction<Object>> functions) {
        SquigglyFunction<Object> winner = null;
        int score = 0;

        for (SquigglyFunction<Object> function : functions) {
            Integer candidateScore = score(functionNode, requestedParameters, function);

            if (candidateScore != null && candidateScore > score) {
                score = candidateScore;
                winner = function;
            }
        }

        return winner;
    }

    private Integer score(FunctionNode functionNode, List<Object> requestedParameters, SquigglyFunction<Object> function) {
        List<SquigglyParameter> configuredParameters = function.getParameters();

        int configuredParametersSize = configuredParameters.size();
        int requestedParametersSize = requestedParameters.size();

        int minLength = configuredParametersSize;
        int maxLength = minLength;
        int varargsIndex = -1;

        if (configuredParametersSize == 0 && requestedParameters.isEmpty()) {
            return 1;
        }

        if (configuredParametersSize > 0 && configuredParameters.get(configuredParametersSize - 1).isVarArgs()) {
            minLength--;
            maxLength = Integer.MAX_VALUE;
            varargsIndex = configuredParametersSize - 1;
        }

        if (requestedParametersSize < minLength) {
            return null;
        }

        if (requestedParametersSize > maxLength) {
            return null;
        }

        int score = 0;

        int end = (varargsIndex < 0) ? configuredParametersSize : varargsIndex;

        for (int i = 0; i < end; i++) {
            SquigglyParameter parameter = configuredParameters.get(i);
            Object requestedParameter = requestedParameters.get(i);
            Integer scoreToAdd = score(parameter.getType(), requestedParameter);

            if (scoreToAdd == null) {
                return null;
            }

            score += scoreToAdd;
        }

        if (varargsIndex >= 0) {
            int scoreToAdd = 0;
            SquigglyParameter varargParameter = configuredParameters.get(varargsIndex);
            Class<?> varargType = MoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());

            for (int i = varargsIndex; i < requestedParametersSize; i++) {
                Integer varargScore = score(varargType, requestedParameters.get(i));

                if (varargScore == null) {
                    return null;
                }

                if (scoreToAdd == 0) {
                    scoreToAdd = varargScore;
                } else if (varargScore < scoreToAdd) {
                    scoreToAdd = varargScore;
                }
            }

            if (scoreToAdd == 0) {
                scoreToAdd = 3;
            }

            score += scoreToAdd;
        }

        return score;
    }

    private Integer score(Class<?> configuredType, Object requestedParameter) {
        if (requestedParameter == null && configuredType.isPrimitive()) {
            return null;
        }

        if (requestedParameter == null) {
            return 1;
        }

        Class<?> requestedType = requestedParameter.getClass();

        if (configuredType.equals(requestedType)) {
            return 3;
        }

        if (squiggly.getConversionService().canConvert(requestedType, configuredType)) {
            return 2;
        }

        return null;
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

    public static void main(String[] args) {
        ObjectMapper mapper = Squiggly.init(new ObjectMapper(), "nickNames.foo(2)");
        System.out.println(SquigglyUtils.stringify(mapper, new Person("Ryan", "Bohn", "rbohn", "bohnman", "doogie")));
    }

    private static class Person {
        private final String firstName;
        private final String lastName;
        private List<String> nickNames;

        public Person(String firstName, String lastName, String... nickNames) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.nickNames = Arrays.asList(nickNames);
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public List<String> getNickNames() {
            return nickNames;
        }
    }
}

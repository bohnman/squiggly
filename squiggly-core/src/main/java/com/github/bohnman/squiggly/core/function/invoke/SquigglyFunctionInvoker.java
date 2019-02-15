package com.github.bohnman.squiggly.core.function.invoke;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.function.FunctionPredicateBridge;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.BaseSquiggly;
import com.github.bohnman.squiggly.core.config.SystemFunctionName;
import com.github.bohnman.squiggly.core.function.FunctionExecutionRequest;
import com.github.bohnman.squiggly.core.function.SquigglyFunction;
import com.github.bohnman.squiggly.core.function.SquigglyParameter;
import com.github.bohnman.squiggly.core.parser.ParseContext;
import com.github.bohnman.squiggly.core.parser.SquigglyParseException;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.core.parser.node.*;
import com.github.bohnman.squiggly.core.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;

/**
 * Contains logic for executing a function.
 */
@SuppressWarnings("unchecked")
public class SquigglyFunctionInvoker {

    private final BaseSquiggly squiggly;
    private final SquigglyFunctionMatcher matcher;
    private final SquigglyVariableResolver variableResolver;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyFunctionInvoker(
            BaseSquiggly squiggly) {
        this(squiggly, squiggly.getVariableResolver());
    }

    private SquigglyFunctionInvoker(
            BaseSquiggly squiggly,
            SquigglyVariableResolver variableResolver) {
        this.matcher = new SquigglyFunctionMatcher(squiggly);
        this.squiggly = notNull(squiggly);
        this.variableResolver = notNull(variableResolver);
    }

    /**
     * Execute the supplied functions in order chaining the result.
     *
     * @param input         object to use as input
     * @param functionNodes nodes
     * @return result
     */
    public Object invoke(@Nullable Object input, Iterable<FunctionNode> functionNodes) {
        return invoke(input, input, null, functionNodes);
    }

    /**
     * Execute the supplied functions in order chaining the result.
     *
     * @param input         object to use as input
     * @param parent        input parent
     * @param functionNodes nodes
     * @return result
     */
    public Object invoke(@Nullable Object input, @Nullable Object child, @Nullable Object parent, Iterable<FunctionNode> functionNodes) {
        input = unwrapJsonNode(input);
        child = unwrapJsonNode(child);
        parent = unwrapJsonNode(parent);

        Object value = input;

        for (FunctionNode functionNode : functionNodes) {
            if (functionNode.isIgnoreNulls() && isNull(value)) {
                break;
            }

            value = invoke(value, child, parent, functionNode);
        }

        return value;
    }

    /**
     * Execute the supplied function.
     *
     * @param input        object to use as input
     * @param functionNode node
     * @return result
     */
    public Object invoke(@Nullable Object input, FunctionNode functionNode) {
        return invoke(input, input, null, functionNode);
    }


    /**
     * Execute the supplied function.
     *
     * @param input        object to use as input
     * @param parent       input parent
     * @param functionNode node
     * @return result
     */
    public Object invoke(@Nullable Object input, Object child, @Nullable Object parent, FunctionNode functionNode) {
        input = unwrapJsonNode(input);
        child = unwrapJsonNode(child);
        parent = unwrapJsonNode(parent);

        if (functionNode.getType().equals(FunctionNodeType.PROPERTY)) {
            return invokeProperty(input, child, parent, functionNode);
        }

        if (functionNode.getType().equals(FunctionNodeType.ASSIGNMENT)) {
            return invokeAssignment(input, child, parent, functionNode);
        }


        return invokeNormalFunction(input, child, parent, functionNode);
    }

    private boolean isNull(Object value) {
        if (value == null) {
            return true;
        }

        if (value instanceof CoreJsonNode) {
            return ((CoreJsonNode) value).isNull();
        }

        return false;
    }

    private Object invokeNormalFunction(Object input, Object child, Object parent, FunctionNode functionNode) {

        List<SquigglyFunction<Object>> functions = squiggly.getFunctionRepository().findByName(functionNode.getName());

        if (functions.isEmpty()) {
            throw new SquigglyParseException(functionNode.getContext(), "Unrecognized function [%s]", functionNode.getName());
        }

        List<Object> parameters = toParameters(functionNode, input, child, parent);

        FunctionMatchRequest request = new FunctionMatchRequest(functionNode, input, child, parent, parameters, functions);
        FunctionMatchResult result = matcher.apply(request);

        SquigglyFunction<Object> winner = result.getWinner();

        if (winner == null) {
            throw new SquigglyParseException(functionNode.getContext(), "Unable to match function [%s] with parameters %s.",
                    functionNode.getName(),
                    parameters.stream()
                            .map(p -> String.format("{type=%s, value=%s}", (p == null ? "null" : p.getClass()), p)).collect(Collectors.toList()));
        }

        parameters = convertParameters(request, result, winner);

        return winner.apply(new FunctionExecutionRequest(input, parameters));
    }

    private boolean isLambdaType(Class<?> type) {
        type = type.getComponentType() == null ? type : type.getComponentType();
        return Predicate.class.isAssignableFrom(type) || Function.class.isAssignableFrom(type);
    }

    private Object invokeAssignment(Object input, Object child, Object parent, FunctionNode functionNode) {
        List<ArgumentNode> argumentNodes = functionNode.getArguments();

        CoreAssert.isTrue(argumentNodes.size() == 2);
        ArgumentNode lastArg = argumentNodes.get(1);

        if (SystemFunctionName.ASSIGN.getFunctionName().equals(functionNode.getName())) {
            if (lastArg.getType() == ArgumentNodeType.FUNCTION_CHAIN) {
                return invoke(input, child, parent, (List<FunctionNode>) lastArg.getValue());
            } else {
                return getValue(lastArg, input, child, parent);
            }
        }

        return invokeNormalFunction(input, child, parent, functionNode);
    }

    private Object invokeProperty(Object input, Object child, Object parent, FunctionNode functionNode) {
        Object key = getValue(functionNode.getArguments().get(1), input, child, parent);

        if (SquigglyParser.SELF_REFERENCE.equals(key)) {
            return child;
        }

        if (SquigglyParser.PARENT_REFERENCE.equals(key)) {
            return parent;
        }

        if (functionNode.isInitial()) {
            input = parent;
        }

        Object object = getValue(functionNode.getArguments().get(0), input, child, parent);

        if (object == null) {
            return null;
        }

        if (object instanceof CoreJsonNode) {
            object = ((CoreJsonNode) object).getValue();
        }

        if (key instanceof Function) {
            return ((Function) key).apply(object);
        }

        if (!squiggly.getFunctionSecurity().isPropertyViewable(key, object.getClass())) {
            return null;
        }

        return CoreBeans.getProperty(object, key);
    }

    private Object unwrapJsonNode(Object input) {
        if (input instanceof CoreJsonNode) {
            return ((CoreJsonNode) input).getValue();
        }

        return input;
    }


    private List<Object> convertParameters(FunctionMatchRequest request, FunctionMatchResult result, SquigglyFunction<Object> winner) {
        List<Object> requestedParameters = result.getParameters();
        List<SquigglyParameter> configuredParameters = winner.getParameters();

        if (configuredParameters.isEmpty()) {
            return Collections.emptyList();
        }

        int requestedParametersSize = requestedParameters.size();
        int configuredParametersSize = configuredParameters.size();
        int varargsIndex = configuredParameters.get(configuredParametersSize - 1).isVarArgs() ? configuredParametersSize - 1 : -1;
        int end = (varargsIndex < 0) ? requestedParametersSize : Math.min(varargsIndex, requestedParametersSize);
        int start = 0;

        List<Object> parameters = new ArrayList<>();

        if (isSquiggly(configuredParameters.get(0))) {
            parameters.add(squiggly);
            start++;
        }

        for (int i = start; i < end; i++) {
            Object requestedParam = requestedParameters.get(i - start);
            SquigglyParameter configuredParam = configuredParameters.get(i);
            parameters.add(convertParameter(request, requestedParam, configuredParam.getType()));
        }

        if (varargsIndex >= 0) {
            SquigglyParameter varargParameter = configuredParameters.get(varargsIndex);
            Class<?> varargType = CoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());
            int len = Math.max(0, requestedParametersSize - varargsIndex + start);
            CoreArrayWrapper array = CoreArrays.wrapperOf(varargType, len);
            start = varargsIndex - start;

            for (int i = start; i < requestedParametersSize; i++) {
                array.set(i - start, convertParameter(request, requestedParameters.get(i), varargType));
            }

            parameters.add(array.getValue());
        }

        return Collections.unmodifiableList(parameters);
    }

    private Object convertParameter(FunctionMatchRequest request, Object source, Class<?> targetType) {
        if ((source instanceof CoreJsonNode) && !CoreJsonNode.class.isAssignableFrom(targetType)) {
            source = ((CoreJsonNode) source).getValue();
        }

        if (source == null) {
            return null;
        }

        if (source instanceof Function && !isLambdaType(targetType)) {
            return convertParameter(request, ((Function) source).apply(request.getInput()), targetType);
        }

        if (targetType.isAssignableFrom(source.getClass())) {
            return source;
        }

        return squiggly.getConversionService().convert(source, targetType);
    }

    private List<Object> toParameters(FunctionNode functionNode, Object input, Object child, Object parent) {
        return functionNode.getArguments()
                .stream()
                .map(argumentNode -> getValue(argumentNode, input, child, parent))
                .collect(Collectors.toList());
    }


    @SuppressWarnings("unchecked")
    private Object getValue(ArgumentNode argumentNode, Object input, Object child, Object parent) {
        switch (argumentNode.getType()) {
            case ARRAY_DECLARATION:
                return buildArrayDeclaration(input, child, parent, (List<ArgumentNode>) argumentNode.getValue());
            case ARRAY_RANGE_DECLARATION:
                return buildArrayRangeDeclartion(input, child, parent, (IntRangeNode) argumentNode.getValue(), argumentNode.getContext());
            case FUNCTION_CHAIN:
                return buildFunctionChain(child, parent, (List<FunctionNode>) argumentNode.getValue());
            case LAMBDA:
                return buildLambda((LambdaNode) argumentNode.getValue());
            case IF:
                return invokeIf(input, child, parent, (IfNode) argumentNode.getValue());
            case INPUT:
                return input;
            case INT_RANGE:
                return buildIntRange(input, child, parent, (IntRangeNode) argumentNode.getValue());
            case OBJECT_DECLARATION:
                return buildObjectDeclaration(input, child, parent, (List<CorePair<ArgumentNode, ArgumentNode>>) argumentNode.getValue());
            case VARIABLE:
                return variableResolver.resolveVariable(argumentNode.getValue().toString());
            default:
                return unwrapJsonNode(argumentNode.getValue());
        }
    }

    private CoreIntRange buildIntRange(Object input, Object child, Object parent, IntRangeNode rangeNode) {
        Integer start = (rangeNode.getStart() == null) ? null : getValue(rangeNode.getStart(), input, child, parent, Integer.class);
        Integer end = (rangeNode.getEnd() == null) ? null : getValue(rangeNode.getEnd(), input, child, parent, Integer.class);

        if (start == null) {
            return rangeNode.isExclusiveEnd() ? CoreIntRange.emptyExclusive() : CoreIntRange.emptyInclusive();
        }

        if (rangeNode.isExclusiveEnd()) {
            return (end == null) ? CoreIntRange.inclusiveExclusive(start) : CoreIntRange.inclusiveExclusive(start, end);
        }

        return (end == null) ? CoreIntRange.inclusiveInclusive(start) : CoreIntRange.inclusiveInclusive(start, end);
    }

    private Object invokeIf(Object input, Object child, Object parent, IfNode ifNode) {
        for (IfNode.IfClause ifClause : ifNode.getIfClauses()) {
            Object condition = invokeAndGetValue(ifClause.getCondition(), input, child, parent);

            if (CoreConversions.toBoolean(condition)) {
                return invokeAndGetValue(ifClause.getValue(), input, child, parent);
            }
        }

        return invokeAndGetValue(ifNode.getElseClause(), input, child, parent);
    }

    private Object invokeAndGetValue(ArgumentNode arg, Object input, Object child, Object parent) {
        Object value = getValue(arg, input, child, parent);

        if (value instanceof Function) {
            return ((Function) value).apply(input);
        }

        return value;
    }

    private List<Object> buildArrayDeclaration(Object input, Object child, Object parent, List<ArgumentNode> elements) {
        return elements.stream().map(arg -> invokeAndGetValue(arg, input, child, parent)).collect(Collectors.toList());
    }

    private Object buildArrayRangeDeclartion(Object input, Object child, Object parent, IntRangeNode rangeNode, ParseContext context) {
        CoreIntRange range = buildIntRange(input, child, parent, rangeNode);

        Integer start = range.getStart();
        Integer end = range.getEnd();

        if (start == null || end == null) {
            return Collections.emptyList();
        }

        if (range.isInclusive()) {
            end = end + 1;
        }

        if (start >= end) {
            return Collections.emptyList();
        }

        int length = end - start;

        if (length > squiggly.getConfig().getMaxArrayRangeDeclarationLength()) {
            throw new SquigglyParseException(context, "Array range declaration cannot exceed a length of %s.", squiggly.getConfig().getMaxArrayRangeDeclarationLength());
        }

        List<Integer> list = new ArrayList<>(length);

        for (int i = start; i < end; i++) {
            list.add(i);
        }

        return list;
    }

    private Function buildFunctionChain(Object child, Object parent, List<FunctionNode> functionNodes) {
        if (functionNodes.isEmpty()) {
            return new FunctionChain(child, parent, Collections.emptyList());
        }

        Function function;
        FunctionNode firstFunctionNode = functionNodes.get(0);

        if (firstFunctionNode.getType() == FunctionNodeType.PROPERTY) {
            function = new Property(child, parent, firstFunctionNode.isAscending(), functionNodes);
        } else {
            function = new FunctionChain(child, parent, functionNodes);
        }

        return function;
    }

    private CoreLambda buildLambda(LambdaNode lambdaNode) {
        return arguments -> {
            if (arguments == null) arguments = new Object[]{};

            List<String> configuredArgs = lambdaNode.getArguments();
            Map<String, Object> varBuilder = new HashMap<>();
            Object input = null;

            if (configuredArgs.size() > 0) {
                int end = Math.min(arguments.length, configuredArgs.size());

                for (int i = 0; i < end; i++) {
                    String name = configuredArgs.get(i);

                    if (!name.equals("_")) {
                        varBuilder.put(name, arguments[i]);
                    }
                }
            } else if (arguments.length > 0) {
                input = arguments[0];
            }

            if (varBuilder.isEmpty()) {
                return invoke(input, input, input, lambdaNode.getBody());
            }

            Map<String, Object> varMap = Collections.unmodifiableMap(varBuilder);

            SquigglyVariableResolver variableResolver = new CompositeVariableResolver(new MapVariableResolver(varMap), this.variableResolver);
            SquigglyFunctionInvoker invoker = new SquigglyFunctionInvoker(squiggly, variableResolver);
            return invoker.invoke(input, input, input, lambdaNode.getBody());
        };
    }

    private Map<Object, Object> buildObjectDeclaration(Object input, Object child, Object parent, List<CorePair<ArgumentNode, ArgumentNode>> pairs) {
        Map<Object, Object> map = new HashMap<>(pairs.size());

        for (CorePair<ArgumentNode, ArgumentNode> pair : pairs) {
            map.put(
                    invokeAndGetValue(pair.getLeft(), input, child, parent),
                    invokeAndGetValue(pair.getRight(), input, child, parent)
            );
        }

        return map;
    }

    @SuppressWarnings("SameParameterValue")
    private <T> T getValue(ArgumentNode argumentNode, Object input, Object child, Object parent, Class<T> targetType) {
        return squiggly.getConversionService().convert(getValue(argumentNode, input, child, parent), targetType);
    }

    private boolean isSquiggly(SquigglyParameter parameter) {
        return !parameter.isVarArgs() && parameter.getType().isAssignableFrom(squiggly.getClass()) && BaseSquiggly.class.isAssignableFrom(parameter.getType());
    }

    private class FunctionChain implements FunctionPredicateBridge {

        private final List<FunctionNode> functionNodes;
        private final Object child;
        private final Object parent;

        public FunctionChain(Object child, Object parent, List<FunctionNode> functionNodes) {
            this.child = child;
            this.parent = parent;
            this.functionNodes = functionNodes;
        }

        @Override
        public Object apply(Object input) {
            if (functionNodes.isEmpty()) return null;
            return invoke(input, child, parent, functionNodes);
        }
    }


    private class Property implements CoreProperty {
        private final boolean ascending;
        private final List<FunctionNode> functionNodes;
        private final Object child;
        private final Object parent;

        public Property(Object child, Object parent, boolean ascending, List<FunctionNode> functionNodes) {
            this.child = child;
            this.parent = parent;
            this.ascending = ascending;
            this.functionNodes = functionNodes;
        }

        @Override
        public boolean isAscending() {
            return ascending;
        }

        @Override
        public Object apply(Object input) {
            return invoke(input, child, parent, functionNodes);
        }
    }

}

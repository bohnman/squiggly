package com.github.bohnman.squiggly.core.function.invoke;

import com.github.bohnman.core.bean.CoreBeans;
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
import com.github.bohnman.squiggly.core.parser.SquigglyParseException;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.core.parser.node.*;
import com.github.bohnman.squiggly.core.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.util.stream.Collectors.toMap;

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
        return invoke(input, null, functionNodes);
    }

    /**
     * Execute the supplied functions in order chaining the result.
     *
     * @param input         object to use as input
     * @param parent        input parent
     * @param functionNodes nodes
     * @return result
     */
    public Object invoke(@Nullable Object input, @Nullable Object parent, Iterable<FunctionNode> functionNodes) {
        Object value = input;

        for (FunctionNode functionNode : functionNodes) {
            if (functionNode.isIgnoreNulls() && isNull(value)) {
                break;
            }

            value = invoke(value, parent, functionNode);
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
        return invoke(input, null, functionNode);
    }


    /**
     * Execute the supplied function.
     *
     * @param input        object to use as input
     * @param parent       input parent
     * @param functionNode node
     * @return result
     */
    public Object invoke(@Nullable Object input, @Nullable Object parent, FunctionNode functionNode) {
        if (functionNode.getType().equals(FunctionNodeType.PROPERTY)) {
            return invokeProperty(input, functionNode);
        }

        if (functionNode.getType().equals(FunctionNodeType.ASSIGNMENT)) {
            return invokeAssignment(parent, parent, functionNode);
        }

        if (functionNode.getType().equals(FunctionNodeType.SELF_ASSIGNMENT)) {
            return invokeAssignment(input, parent, functionNode);
        }

        return invokeNormalFunction(input, functionNode);
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

    private Object invokeNormalFunction(Object input, FunctionNode functionNode) {

        List<SquigglyFunction<Object>> functions = squiggly.getFunctionRepository().findByName(functionNode.getName());

        if (functions.isEmpty()) {
            throw new SquigglyParseException(functionNode.getContext(), "Unrecognized function [%s]", functionNode.getName());
        }

        List<Object> parameters = toParameters(functionNode, input);

        FunctionMatchRequest request = new FunctionMatchRequest(functionNode, input, parameters, functions);
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

    private Object invokeAssignment(Object input, Object parent, FunctionNode functionNode) {
        List<ArgumentNode> argumentNodes = functionNode.getArguments();

        CoreAssert.isTrue(argumentNodes.size() == 2);
        ArgumentNode lastArg = argumentNodes.get(1);

        if (SystemFunctionName.ASSIGN.getFunctionName().equals(functionNode.getName())) {
            if (lastArg.getType() == ArgumentNodeType.FUNCTION_CHAIN) {
                return invoke(input, parent, (List<FunctionNode>) lastArg.getValue());
            } else {
                return getValue(lastArg, input);
            }
        }

        return invokeNormalFunction(input, functionNode);
    }

    private Object invokeProperty(Object input, FunctionNode functionNode) {
        Object object = getValue(functionNode.getArguments().get(0), input);
        Object key = getValue(functionNode.getArguments().get(1), input);

        if (SquigglyParser.OP_DOLLAR.equals(key)) {
            return input;
        }

        if (object == null) {
            return null;
        }

        if (object instanceof CoreJsonNode) {
            object = ((CoreJsonNode) input).getValue();
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
        int configuredAdder = 0;

        List<Object> parameters = new ArrayList<>();

        if (isSquiggly(configuredParameters.get(0))) {
            parameters.add(squiggly);
            configuredAdder++;
        }

        for (int i = 0; i < end; i++) {
            Object requestedParam = requestedParameters.get(i);
            SquigglyParameter configuredParam = configuredParameters.get(i + configuredAdder);
            parameters.add(convertParameter(requestedParam, configuredParam.getType()));
        }

        if (varargsIndex >= 0) {
            SquigglyParameter varargParameter = configuredParameters.get(varargsIndex);
            Class<?> varargType = CoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());
            int len = Math.max(0, requestedParametersSize - varargsIndex);
            Object[] array = CoreArrays.newArray(varargType, len);

            for (int i = varargsIndex; i < requestedParametersSize; i++) {
                array[i - varargsIndex] = convertParameter(requestedParameters.get(i), varargType);
            }

            parameters.add(array);
        }

        return Collections.unmodifiableList(parameters);
    }

    private Object convertParameter(Object source, Class<?> targetType) {
        if ((source instanceof CoreJsonNode) && !CoreJsonNode.class.isAssignableFrom(targetType)) {
            source = ((CoreJsonNode) source).getValue();
        }

        if (source == null) {
            return null;
        }

        if (targetType.isAssignableFrom(source.getClass())) {
            return source;
        }

        return squiggly.getConversionService().convert(source, targetType);
    }

    private List<Object> toParameters(FunctionNode functionNode, Object input) {
        return functionNode.getArguments()
                .stream()
                .map(argumentNode -> getValue(argumentNode, input))
                .collect(Collectors.toList());
    }


    @SuppressWarnings("unchecked")
    private Object getValue(ArgumentNode argumentNode, Object input) {
        switch (argumentNode.getType()) {
            case ARRAY_DECLARATION:
                return buildArrayDeclaration(input, (List<ArgumentNode>) argumentNode.getValue());
            case FUNCTION_CHAIN:
                return buildFunctionChain((List<FunctionNode>) argumentNode.getValue());
            case LAMBDA:
                return buildLambda((LambdaNode) argumentNode.getValue());
            case IF:
                return invokeIf((IfNode) argumentNode.getValue(), input);
            case INPUT:
                return input;
            case INT_RANGE:
                IntRangeNode rangeNode = (IntRangeNode) argumentNode.getValue();
                Integer start = (rangeNode.getStart() == null) ? null : getValue(rangeNode.getStart(), input, Integer.class);
                Integer end = (rangeNode.getEnd() == null) ? null : getValue(rangeNode.getEnd(), input, Integer.class);

                if (start == null) {
                    return rangeNode.isExclusiveEnd() ? CoreIntRange.emptyExclusive() : CoreIntRange.emptyInclusive();
                }

                if (rangeNode.isExclusiveEnd()) {
                    return (end == null) ? CoreIntRange.inclusiveExclusive(start) : CoreIntRange.inclusiveExclusive(start, end);
                }

                return (end == null) ? CoreIntRange.inclusiveInclusive(start) : CoreIntRange.inclusiveInclusive(start, end);
            case OBJECT_DECLARATION:
                return buildObjectDeclaration(input, (List<CorePair<ArgumentNode, ArgumentNode>>) argumentNode.getValue());
            case VARIABLE:
                return variableResolver.resolveVariable(argumentNode.getValue().toString());
            default:
                return unwrapJsonNode(argumentNode.getValue());
        }
    }

    private Object invokeIf(IfNode ifNode, Object input) {
        for (IfNode.IfClause ifClause : ifNode.getIfClauses()) {
            Object condition = invokeAndGetValue(ifClause.getCondition(), input);

            if (CoreConversions.toBoolean(condition)) {
                return invokeAndGetValue(ifClause.getValue(), input);
            }
        }

        return invokeAndGetValue(ifNode.getElseClause(), input);
    }

    private Object invokeAndGetValue(ArgumentNode arg, Object input) {
        Object value = getValue(arg, input);

        if (value instanceof Function) {
            return ((Function) value).apply(input);
        }

        return value;
    }

    private List<Object> buildArrayDeclaration(Object input, List<ArgumentNode> elements) {
        return elements.stream().map(arg -> invokeAndGetValue(arg, input)).collect(Collectors.toList());
    }

    private Function buildFunctionChain(List<FunctionNode> functionNodes) {
        if (functionNodes.isEmpty()) {
            return new FunctionChain(Collections.emptyList());
        }

        Function function;
        FunctionNode firstFunctionNode = functionNodes.get(0);

        if (firstFunctionNode.getType() == FunctionNodeType.PROPERTY) {
            function = new Property(firstFunctionNode.isAscending(), functionNodes);
        } else {
            function = new FunctionChain(functionNodes);
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
                return invoke(input, lambdaNode.getBody());
            }

            Map<String, Object> varMap = Collections.unmodifiableMap(varBuilder);

            SquigglyVariableResolver variableResolver = new CompositeVariableResolver(new MapVariableResolver(varMap), this.variableResolver);
            SquigglyFunctionInvoker invoker = new SquigglyFunctionInvoker(squiggly, variableResolver);
            return invoker.invoke(input, lambdaNode.getBody());
        };
    }

    private Map<Object, Object> buildObjectDeclaration(Object input, List<CorePair<ArgumentNode, ArgumentNode>> pairs) {
        return pairs.stream()
                .collect(toMap(
                        pair -> invokeAndGetValue(pair.getLeft(), input),
                        pair -> invokeAndGetValue(pair.getRight(), input),
                        (a, b) -> b
                ));
    }

    @SuppressWarnings("SameParameterValue")
    private <T> T getValue(ArgumentNode argumentNode, Object input, Class<T> targetType) {
        return squiggly.getConversionService().convert(getValue(argumentNode, input), targetType);
    }

    private boolean isSquiggly(SquigglyParameter parameter) {
        return !parameter.isVarArgs() && parameter.getType().isAssignableFrom(squiggly.getClass()) && BaseSquiggly.class.isAssignableFrom(parameter.getType());
    }

    private class FunctionChain implements FunctionPredicateBridge {

        private final List<FunctionNode> functionNodes;

        public FunctionChain(List<FunctionNode> functionNodes) {
            this.functionNodes = functionNodes;
        }

        @Override
        public Object apply(Object input) {
            if (functionNodes.isEmpty()) return null;
            return invoke(input, functionNodes);
        }
    }


    private class Property implements CoreProperty {
        private final boolean ascending;
        private final List<FunctionNode> functionNodes;

        public Property(boolean ascending, List<FunctionNode> functionNodes) {
            this.ascending = ascending;
            this.functionNodes = functionNodes;
        }

        @Override
        public boolean isAscending() {
            return ascending;
        }

        @Override
        public Object apply(Object input) {
            return invoke(input, functionNodes);
        }
    }

}

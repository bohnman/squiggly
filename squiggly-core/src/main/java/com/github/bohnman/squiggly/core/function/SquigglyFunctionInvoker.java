package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.function.FunctionPredicateBridge;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.config.SystemFunctionName;
import com.github.bohnman.squiggly.core.convert.ConverterRecord;
import com.github.bohnman.squiggly.core.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.parser.ArgumentNode;
import com.github.bohnman.squiggly.core.parser.ArgumentNodeType;
import com.github.bohnman.squiggly.core.parser.FunctionNode;
import com.github.bohnman.squiggly.core.parser.FunctionNodeType;
import com.github.bohnman.squiggly.core.parser.IfNode;
import com.github.bohnman.squiggly.core.parser.IntRangeNode;
import com.github.bohnman.squiggly.core.parser.LambdaNode;
import com.github.bohnman.squiggly.core.parser.SquigglyParseException;
import com.github.bohnman.squiggly.core.parser.SquigglyParser;
import com.github.bohnman.squiggly.core.variable.CompositeVariableResolver;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static java.util.stream.Collectors.toMap;

@SuppressWarnings("unchecked")
public class SquigglyFunctionInvoker {

    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyVariableResolver variableResolver;
    private final SquigglyConversionService conversionService;

    public SquigglyFunctionInvoker(
            SquigglyConversionService conversionService,
            SquigglyFunctionRepository functionRepository,
            SquigglyVariableResolver variableResolver) {
        this.conversionService = notNull(conversionService);
        this.functionRepository = notNull(functionRepository);
        this.variableResolver = notNull(variableResolver);
    }

    public Object invoke(@Nullable Object input, Iterable<FunctionNode> functionNodes) {
        return invoke(input, null, functionNodes);
    }

    public Object invoke(@Nullable Object input, @Nullable Object parent, Iterable<FunctionNode> functionNodes) {
        Object value = input;

        for (FunctionNode functionNode : functionNodes) {
            if (functionNode.isIgnoreNulls() && value == null) {
                break;
            }

            value = invoke(value, parent, functionNode);
        }

        return value;
    }

    public Object invoke(@Nullable Object input, FunctionNode functionNode) {
        return invoke(input, null, functionNode);
    }


    public Object invoke(@Nullable Object input, @Nullable Object parent, FunctionNode functionNode) {
        if (functionNode.getType().equals(FunctionNodeType.PROPERTY)) {
            return invokeProperty(input, parent, functionNode);
        }

        if (functionNode.getType().equals(FunctionNodeType.ASSIGNMENT)) {
            return invokeAssignment(parent, parent, functionNode);
        }

        if (functionNode.getType().equals(FunctionNodeType.SELF_ASSIGNMENT)) {
            return invokeAssignment(input, parent, functionNode);
        }

        return invokeNormalFunction(input, parent, functionNode);
    }

    private Object invokeNormalFunction(Object input, Object parent, FunctionNode functionNode) {

        List<SquigglyFunction<Object>> functions = functionRepository.findByName(functionNode.getName());

        if (functions.isEmpty()) {
            throw new SquigglyParseException(functionNode.getContext(), "Unrecognized function [%s]", functionNode.getContext(), functionNode.getName());
        }

        List<Object> requestedParameters = toParameters(functionNode, input);
        SquigglyFunction<Object> winner = findBestCandidateFunction(functionNode, input, requestedParameters, functions);

        if (winner == null) {
            throw new SquigglyParseException(functionNode.getContext(), "Unable to match function [%s] with parameters %s.",
                    functionNode.getName(),
                    requestedParameters.stream()
                            .map(p -> String.format("{type=%s, value=%s}", (p == null ? "null" : p.getClass()), p)).collect(Collectors.toList()));
        }

        List<Object> parameters = convert(requestedParameters, winner);

        return winner.apply(new FunctionRequest(input, parameters));
    }

    private Object invokeAssignment(Object input, Object parent, FunctionNode functionNode) {
        List<ArgumentNode> argumentNodes = functionNode.getParameters();

        CoreAssert.isTrue(argumentNodes.size() == 2);
        ArgumentNode lastArg = argumentNodes.get(1);

        if (SystemFunctionName.ASSIGN.getFunctionName().equals(functionNode.getName())) {
            if (lastArg.getType() == ArgumentNodeType.FUNCTION_CHAIN) {
                return invoke(input, parent, (List<FunctionNode>) lastArg.getValue());
            } else {
                return getValue(lastArg, input);
            }
        }

        return invokeNormalFunction(input, parent, functionNode);
    }

    private Object invokeProperty(Object input, Object parent, FunctionNode functionNode) {
        Object object = getValue(functionNode.getParameters().get(0), input);
        Object key = getValue(functionNode.getParameters().get(1), input);

        if (SquigglyParser.OP_DOLLAR.equals(key)) {
            return input;
        }

        if (key instanceof Function) {
            return ((Function) key).apply(object);
        }

        return CoreBeans.getProperty(object, key);
    }

    private SquigglyFunction<Object> findBestCandidateFunction(FunctionNode functionNode, Object input, List<Object> requestedParameters, List<SquigglyFunction<Object>> functions) {
        SquigglyFunction<Object> winner = null;
        Score score = null;

        for (SquigglyFunction<Object> function : functions) {

            Score candidateScore = new Score();
            boolean candidateScored = score(candidateScore, functionNode, input, requestedParameters, function);

            if (candidateScored && (score == null || candidateScore.compareTo(score) > 0)) {
                score = candidateScore;
                winner = function;
            }
        }


        return winner;
    }

    private boolean score(Score score, FunctionNode functionNode, Object input, List<Object> requestedParameters, SquigglyFunction<Object> function) {
        List<SquigglyParameter> configuredParameters = function.getParameters();

        int configuredParametersSize = configuredParameters.size();
        int requestedParametersSize = requestedParameters.size();

        int minLength = configuredParametersSize;
        int maxLength = minLength;
        int varargsIndex = -1;

        if (configuredParametersSize == 0 && requestedParameters.isEmpty()) {
            score.base();
            return true;
        }

        if (configuredParametersSize > 0 && configuredParameters.get(configuredParametersSize - 1).isVarArgs()) {
            minLength--;
            maxLength = Integer.MAX_VALUE;
            varargsIndex = configuredParametersSize - 1;
        }

        if (requestedParametersSize < minLength) {
            return false;
        }

        if (requestedParametersSize > maxLength) {
            return false;
        }

        int end = (varargsIndex < 0) ? configuredParametersSize : varargsIndex;

        for (int i = 0; i < end; i++) {
            SquigglyParameter parameter = configuredParameters.get(i);
            boolean scored = score(score, input, parameter.getType(), requestedParameters, i);

            if (!scored) {
                return false;
            }
        }

        if (varargsIndex >= 0) {
            Score varargScore = null;
            SquigglyParameter varargParameter = configuredParameters.get(varargsIndex);
            Class<?> varargType = CoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());

            for (int i = varargsIndex; i < requestedParametersSize; i++) {
                Score candidateScore = new Score();
                boolean scored = score(candidateScore, input, varargType, requestedParameters, i);

                if (!scored) {
                    return false;
                }

                if (varargScore == null || candidateScore.compareTo(varargScore) > 0) {
                    varargScore = candidateScore;
                }
            }

            if (varargScore == null) {
                if (requestedParameters.isEmpty()) score.base();
            } else {
                score.add(varargScore);
            }
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    private boolean score(Score score, Object input, Class<?> configuredType, List<Object> requestedParameters, int index) {
        Object requestedParameter = requestedParameters.get(index);

        if (requestedParameter == null && configuredType.isPrimitive()) {
            return false;
        }

        if (requestedParameter == null) {
            score.base();
            return true;
        }

        Class<?> requestedType = requestedParameter.getClass();

        if (configuredType.equals(requestedType)) {
            score.exact();
            return true;
        }

        if (configuredType.equals(CoreLambda.class) && CoreLambda.class.isAssignableFrom(requestedType)) {
            score.exact();
            return true;
        }

        if (configuredType.equals(CoreProperty.class) && CoreProperty.class.isAssignableFrom(requestedType)) {
            score.exact();
            return true;
        }

        if (Function.class.isAssignableFrom(requestedType) && !typeIsLambda(configuredType)) {
            requestedParameters.set(index, ((Function) requestedParameter).apply(input));
            return score(score, input, configuredType, requestedParameters, index);
        }

        if (Predicate.class.isAssignableFrom(requestedType) && !typeIsLambda(configuredType)) {
            requestedParameters.set(index, ((Predicate) requestedParameter).test(input));
            return score(score, input, configuredType, requestedParameters, index);
        }

        if (configuredType.isAssignableFrom(requestedType)) {
            score.assignable();
            return true;
        }

        ConverterRecord record = conversionService.findRecord(requestedType, configuredType);

        if (record == null) {
            return false;
        }

        score.convertible(record.getOrder());
        return true;
    }

    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    private boolean typeIsLambda(Class<?> type) {
        return Predicate.class.isAssignableFrom(type) || Function.class.isAssignableFrom(type);
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
            Class<?> varargType = CoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());
            int len = Math.max(0, requestedParametersSize - varargsIndex);
            Object[] array = CoreArrays.newArray(varargType, len);

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

        if (type.isAssignableFrom(requestedParam.getClass())) {
            return requestedParam;
        }

        return conversionService.convert(requestedParam, type);
    }

    private List<Object> toParameters(FunctionNode functionNode, Object input) {
        return functionNode.getParameters()
                .stream()
                .map(argumentNode -> toParameter(functionNode, input, argumentNode))
                .collect(Collectors.toList());
    }

    private Object toParameter(FunctionNode functionNode, Object input, ArgumentNode argumentNode) {
        return getValue(argumentNode, input);
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
                return argumentNode.getValue();
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

    private Map<Object, Object> buildObjectDeclaration(Object input, List<CorePair<ArgumentNode, ArgumentNode>> pairs) {
        return pairs.stream()
                .collect(toMap(
                        pair -> invokeAndGetValue(pair.getLeft(), input),
                        pair -> invokeAndGetValue(pair.getRight(), input),
                        (a, b) -> b
                ));
    }

    private List<Object> buildArrayDeclaration(Object input, List<ArgumentNode> elements) {
        return elements.stream().map(arg -> invokeAndGetValue(arg, input)).collect(Collectors.toList());
    }

    private Object invokeAndGetValue(ArgumentNode arg, Object input) {
        Object value = getValue(arg, input);

        if (value instanceof Function) {
            return ((Function) value).apply(input);
        }

        return value;
    }

    private Function buildFunctionChain(List<FunctionNode> functionNodes) {
        if (functionNodes.isEmpty()) {
            return args -> null;
        }

        Function function;
        FunctionNode firstFunctionNode = functionNodes.get(0);

        if (firstFunctionNode.getType() == FunctionNodeType.PROPERTY) {
            function = new CoreProperty() {
                @Override
                public boolean isAscending() {
                    return firstFunctionNode.isAscending();
                }

                @Override
                public Object apply(Object input) {
                    return invoke(input, functionNodes);
                }
            };
        } else {
            function = (FunctionPredicateBridge) input -> invoke(input, functionNodes);
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
            SquigglyFunctionInvoker invoker = new SquigglyFunctionInvoker(conversionService, functionRepository, variableResolver);
            return invoker.invoke(input, lambdaNode.getBody());
        };
    }

    private <T> T getValue(ArgumentNode argumentNode, Object input, Class<T> targetType) {
        return conversionService.convert(getValue(argumentNode, input), targetType);
    }

    private static class Score implements Comparable<Score> {
        private int exactMatches;
        private int assignableMatches;
        private int convertibleOrders;
        private int baseMatches;

        public Score() {
        }

        private Score exact() {
            exactMatches++;
            return this;
        }

        private Score assignable() {
            assignableMatches++;
            return this;

        }

        private Score convertible(int order) {
            convertibleOrders += order;
            return this;

        }

        private Score base() {
            baseMatches++;
            return this;
        }

        @Override
        public int compareTo(Score o) {
            int cmp;
            cmp = Integer.compare(exactMatches, o.exactMatches);
            if (cmp != 0) return cmp;
            cmp = Integer.compare(assignableMatches, o.assignableMatches);
            if (cmp != 0) return cmp;
            cmp = -1 * Integer.compare(convertibleOrders, o.convertibleOrders);
            if (cmp != 0) return cmp;
            cmp = Integer.compare(baseMatches, o.baseMatches);
            return cmp;
        }

        private int compare(List<Integer> l1, List<Integer> l2) {
            return 0;
        }

        public void add(Score score) {
            exactMatches += score.exactMatches;
            assignableMatches += score.assignableMatches;
            convertibleOrders += score.convertibleOrders;
            baseMatches += score.baseMatches;
        }
    }
}

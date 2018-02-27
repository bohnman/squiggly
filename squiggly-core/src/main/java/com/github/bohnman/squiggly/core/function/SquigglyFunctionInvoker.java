package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.function.FunctionPredicateBridge;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.core.range.CoreIntRange;
import com.github.bohnman.squiggly.core.convert.ConverterRecord;
import com.github.bohnman.squiggly.core.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.parser.ArgumentNode;
import com.github.bohnman.squiggly.core.parser.FunctionNode;
import com.github.bohnman.squiggly.core.parser.FunctionNodeType;
import com.github.bohnman.squiggly.core.parser.IntRangeNode;
import com.github.bohnman.squiggly.core.parser.LambdaNode;
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
import static java.lang.String.format;

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

    public Object invoke(Object input, Iterable<FunctionNode> functionNodes) {
        Object value = input;

        for (FunctionNode functionNode : functionNodes) {
            if (functionNode.isIgnoreNulls() && value == null) {
                break;
            }

            value = invoke(value, functionNode);
        }

        return value;
    }

    public Object invoke(@Nullable Object input, FunctionNode functionNode) {
        if (functionNode.getType().equals(FunctionNodeType.PROPERTY)) {
            return invokeProperty(input, functionNode);
        }


        List<SquigglyFunction<Object>> functions = functionRepository.findByName(functionNode.getName());

        if (functions.isEmpty()) {
            throw new IllegalStateException(format("%s: Unrecognized function [%s]", functionNode.getContext(), functionNode.getName()));
        }

        List<Object> requestedParameters = toParameters(functionNode, input);
        SquigglyFunction<Object> winner = findBestCandidateFunction(functionNode, input, requestedParameters, functions);

        if (winner == null) {
            throw new IllegalStateException(format("%s: Unable to match function [%s] with parameters %s.",
                    functionNode.getContext(),
                    functionNode.getName(),
                    requestedParameters.stream()
                            .map(p -> String.format("{type=%s, value=%s}", (p == null ? "null" : p.getClass()), p)).collect(Collectors.toList())));
        }

        List<Object> parameters = convert(requestedParameters, winner);

        return winner.apply(new FunctionRequest(input, parameters));

    }

    private Object invokeProperty(Object input, FunctionNode functionNode) {
        Object object = getValue(functionNode.getParameters().get(0), input);
        Object key = getValue(functionNode.getParameters().get(1), input);

        if ("@".equals(key)) {
            return input;
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
            case FUNCTION_CHAIN:
                return buildFunctionChain((List<FunctionNode>) argumentNode.getValue());
            case LAMBDA:
                return buildLambda((LambdaNode) argumentNode.getValue());
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
            case VARIABLE:
                return variableResolver.resolveVariable(argumentNode.getValue().toString());
            default:
                return argumentNode.getValue();
        }
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

            if (configuredArgs.size() > 0) {
                int end = Math.min(arguments.length, configuredArgs.size());

                for (int i = 0; i < end; i++) {
                    String name = configuredArgs.get(i);

                    if (!name.equals("_")) {
                        varBuilder.put(name, arguments[i]);
                    }
                }
            } else if (arguments.length > 0) {
                varBuilder.put("it", arguments[0]);
            }

            if (varBuilder.isEmpty()) {
                return invoke(null, lambdaNode.getBody());
            }

            Map<String, Object> varMap = Collections.unmodifiableMap(varBuilder);

            SquigglyVariableResolver variableResolver = new CompositeVariableResolver(new MapVariableResolver(varMap), this.variableResolver);
            SquigglyFunctionInvoker invoker = new SquigglyFunctionInvoker(conversionService, functionRepository, variableResolver);
            return invoker.invoke(null, lambdaNode.getBody());
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

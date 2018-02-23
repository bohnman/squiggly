package com.github.bohnman.squiggly.filter.function;

import com.github.bohnman.squiggly.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.function.FunctionRequest;
import com.github.bohnman.squiggly.function.SquigglyFunction;
import com.github.bohnman.squiggly.function.SquigglyParameter;
import com.github.bohnman.squiggly.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.parser.FunctionNode;
import com.github.bohnman.squiggly.parser.ArgumentNode;
import com.github.bohnman.squiggly.variable.SquigglyVariableResolver;
import com.google.common.base.MoreObjects;
import com.google.common.collect.ObjectArrays;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;
import static java.lang.String.format;

public class FunctionInvoker {

    private static final int SCORE_NULL = 1;
    private static final int SCORE_CONVERT = 2;
    private static final int SCORE_EXACT = 4;

    private final SquigglyFunctionRepository functionRepository;
    private final SquigglyVariableResolver variableResolver;
    private final SquigglyConversionService conversionService;

    public FunctionInvoker(
            SquigglyConversionService conversionService,
            SquigglyFunctionRepository functionRepository,
            SquigglyVariableResolver variableResolver) {
        this.conversionService = checkNotNull(conversionService);
        this.functionRepository = checkNotNull(functionRepository);
        this.variableResolver = checkNotNull(variableResolver);
    }

    public Object invoke(FunctionNode functionNode, Object input) {
        List<SquigglyFunction<Object>> functions = functionRepository.findByName(functionNode.getName());

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
                scoreToAdd = SCORE_EXACT;
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
            return SCORE_NULL;
        }

        Class<?> requestedType = requestedParameter.getClass();

        if (configuredType.equals(requestedType)) {
            return SCORE_EXACT;
        }

        if (conversionService.canConvert(requestedType, configuredType)) {
            return SCORE_CONVERT;
        }

        return null;
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

        return conversionService.convert(requestedParam, type);
    }

    private List<Object> toParameters(FunctionNode functionNode, Object input) {
        return functionNode.getParameters()
                .stream()
                .map(argumentNode -> toParameter(functionNode, input, argumentNode))
                .collect(Collectors.toList());
    }

    private Object toParameter(FunctionNode functionNode, Object input, ArgumentNode argumentNode) {
        switch (argumentNode.getType()) {
            case INPUT:
                return input;
            case VARIABLE:
                return variableResolver.resolveVariable(argumentNode.getValue().toString());
            default:
                return argumentNode.getValue();
        }
    }
}

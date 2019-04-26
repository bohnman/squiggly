package com.github.bohnman.squiggly.function;

import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.function.CoreProperty;
import com.github.bohnman.core.function.FunctionPredicateBridge;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.squiggly.engine.SquigglyEngine;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.function.FunctionMatchResult.Score;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static java.util.stream.Collectors.toList;

/**
 * Main class containing the logic for matching functions.
 */
public class SquigglyFunctionMatcher implements Function<FunctionMatchRequest, FunctionMatchResult> {

    private final SquigglyEngine squiggly;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyFunctionMatcher(SquigglyEngine squiggly) {
        this.squiggly = CoreAssert.notNull(squiggly);
    }

    /**
     * Perform the matching.
     *
     * @param request request
     * @return result
     */
    @Override
    public FunctionMatchResult apply(FunctionMatchRequest request) {
        List<SquigglyFunction<Object>> functions = filterForNumberOfArguments(request);
        List<SquigglyFunction<Object>> lazyFunctions = new ArrayList<>(functions.size());
        List<SquigglyFunction<Object>> eagerFunctions = new ArrayList<>(functions.size());

        for (SquigglyFunction<Object> function : functions) {
            if (isLazy(request, function)) {
                lazyFunctions.add(function);
            } else {
                eagerFunctions.add(function);
            }
        }

        FunctionMatchResult result = new FunctionMatchResult(request);

        if (score(request, result, lazyFunctions).isNotEmpty()) {
            return result;
        }

        score(request, result, eagerFunctions);
        return result;
    }

    private boolean isLazy(FunctionMatchRequest request, SquigglyFunction<Object> function) {
        if (function.getParameters().isEmpty()) {
            return false;
        }

        for (int i = 0; i < function.getParameters().size(); i++) {
            SquigglyParameter configuredParameter = function.getParameters().get(i);

            if (isLambdaType(configuredParameter.getType())) {
                return true;
            }
        }

        return false;
    }

    private Score score(FunctionMatchRequest request, FunctionMatchResult result, List<SquigglyFunction<Object>> functions) {
        Score highScore = Score.EMPTY;

        for (SquigglyFunction<Object> function : functions) {
            Score score = score(request, result, function);

            if (score.isEmpty()) {
                continue;
            }

            if (score.compareTo(highScore) > 0) {
                highScore = score;
                result.setWinner(function);
            }
        }

        result.setScore(highScore);
        return highScore;
    }

    private Score score(FunctionMatchRequest request, FunctionMatchResult result, SquigglyFunction<Object> function) {
        boolean requestParamsEmpty = request.getParameters().isEmpty();
        boolean functionParamsEmpty = function.getParameters().isEmpty();

        Score score = new Score();

        if (functionParamsEmpty) {
            return requestParamsEmpty ? score.exact() : Score.EMPTY;
        }

        int start = 0;
        int end = function.getParameters().size();
        int requestParamIncrement = 0;

        if (isSquiggly(function.getParameters().get(0))) {
            start = 1;
            requestParamIncrement = -1;
        }

        int varargsIndex = -1;

        if (function.getParameters().get(end - 1).isVarArgs()) {
            varargsIndex = end - 1;
            end--;
        }

        for (int i = start; i < end; i++) {
            Class<?> configuredType = function.getParameters().get(i).getType();
            score = applyScore(score, request, result, configuredType, i + requestParamIncrement);

            if (score.isEmpty()) {
                return score;
            }
        }

        if (varargsIndex > -1) {
            SquigglyParameter varargParameter = function.getParameters().get(varargsIndex);
            Class<?> varargType = CoreObjects.firstNonNull(varargParameter.getType().getComponentType(), varargParameter.getType());

            for (int i = varargsIndex; i < result.getParameters().size(); i++) {
                score = applyScore(score, request, result, varargType, i);

                if (score.isEmpty()) {
                    return score;
                }
            }
        }

        return score;
    }

    @SuppressWarnings("unchecked")
    private Score applyScore(Score score, FunctionMatchRequest request, FunctionMatchResult result, Class<?> configuredType, int index) {
        Object requestedParameter = result.getParameters().get(index);

        if (requestedParameter == null && configuredType.isPrimitive()) {
            return Score.EMPTY;
        }

        if (requestedParameter == null) {
            score.undefined(computeDistance(configuredType, Object.class));
            return score;
        }

        Class<?> requestedType = requestedParameter.getClass();

        if (Function.class.isAssignableFrom(requestedType) && !isLambdaType(configuredType)) {
            result.getParameters().set(index, ((Function) result.getParameters().get(index)).apply(result.getInput()));
            return applyScore(score, request, result, configuredType, index);
        }

        if (Predicate.class.isAssignableFrom(requestedType) && !isLambdaType(configuredType)) {
            result.getParameters().set(index, ((Predicate) result.getParameters().get(index)).test(result.getInput()));
            return applyScore(score, request, result, configuredType, index);
        }

        return applyNormalScore(score, request, result, configuredType, index, requestedType);
    }

    private Score applyNormalScore(Score score, FunctionMatchRequest request, FunctionMatchResult result, Class<?> configuredType, int index, Class<?> requestedType) {
        if (CoreJsonNode.class.isAssignableFrom(requestedType) && !CoreJsonNode.class.isAssignableFrom(configuredType)) {
            requestedType = ((CoreJsonNode) request.getParameters().get(index)).getValue().getClass();
        }

        if (configuredType.isAssignableFrom(requestedType)) {
            score.assignable(computeDistance(requestedType, configuredType));
            return score;
        }

        ConverterRecord record = squiggly.getConversionService().findRecord(requestedType, configuredType);

        if (record == null) {
            return Score.EMPTY;
        }

        return score.convertible(record.getOrder());
    }


    private int computeDistance(Class<?> sourceClass, Class<?> targetClass) {
        if (targetClass.equals(CoreLambda.class) || targetClass.equals(CoreProperty.class)) {
            return 0;
        }

        if (targetClass.equals(Function.class)) {
            if (FunctionPredicateBridge.class.isAssignableFrom(sourceClass)) {
                return 1;
            }

            return 0;
        }

        if (targetClass.equals(Predicate.class)) {
            if (FunctionPredicateBridge.class.isAssignableFrom(sourceClass)) {
                // prefer function over predicate
                return 2;
            }

            return 0;
        }

        if (targetClass.equals(CoreJsonNode.class)) {
            return 0;
        }

        return computeDistance(sourceClass, targetClass, 0);
    }

    private int computeDistance(Class<?> sourceClass, Class<?> targetClass, int distance) {
        if (sourceClass == null || sourceClass == targetClass || sourceClass == Object.class) {
            return distance;
        }

        distance = computeDistance(sourceClass.getSuperclass(), targetClass, distance + 1);

        for (Class<?> iClass : sourceClass.getInterfaces()) {
            distance = Math.min(distance, computeDistance(iClass, targetClass, distance + 1));
        }

        return distance;
    }


    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    private boolean isLambdaType(Class<?> type) {
        type = type.getComponentType() == null ? type : type.getComponentType();
        return Predicate.class.isAssignableFrom(type) || Function.class.isAssignableFrom(type);
    }

    private List<SquigglyFunction<Object>> filterForNumberOfArguments(FunctionMatchRequest request) {
        return request.getFunctions().stream().filter(function -> meetsRequiredNumberOfArguments(request, function)).collect(toList());
    }

    private boolean meetsRequiredNumberOfArguments(FunctionMatchRequest request, SquigglyFunction<Object> function) {
        List<SquigglyParameter> configuredParameters = function.getParameters();

        int configuredParametersSize = configuredParameters.size();

        if (configuredParametersSize > 0 && isSquiggly(configuredParameters.get(0))) {
            configuredParametersSize--;
        }

        int requestedParametersSize = request.getParameters().size();

        int minLength = configuredParametersSize;
        int maxLength = minLength;
        int varargsIndex = -1;

        if (configuredParametersSize > 0 && configuredParameters.get(configuredParametersSize - 1).isVarArgs()) {
            minLength--;
            maxLength = Integer.MAX_VALUE;
        }

        return requestedParametersSize >= minLength && requestedParametersSize <= maxLength;
    }

    private boolean isSquiggly(SquigglyParameter parameter) {
        return !parameter.isVarArgs() && parameter.getType().isAssignableFrom(squiggly.getClass()) && SquigglyEngine.class.isAssignableFrom(parameter.getType());
    }

}

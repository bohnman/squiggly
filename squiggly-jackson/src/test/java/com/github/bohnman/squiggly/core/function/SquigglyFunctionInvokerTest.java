package com.github.bohnman.squiggly.core.function;

import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.convert.DefaultConversionService;
import com.github.bohnman.squiggly.core.convert.DefaultConverters;
import com.github.bohnman.squiggly.core.convert.SquigglyConversionService;
import com.github.bohnman.squiggly.core.convert.SquigglyConverterRegistry;
import com.github.bohnman.squiggly.core.convert.joda.SquigglyConverterRegistries;
import com.github.bohnman.squiggly.core.function.invoke.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.core.function.repository.MapFunctionRepository;
import com.github.bohnman.squiggly.core.function.repository.SquigglyFunctionRepository;
import com.github.bohnman.squiggly.core.parse.node.ArgumentNode;
import com.github.bohnman.squiggly.core.parse.node.ArgumentNodeType;
import com.github.bohnman.squiggly.core.parse.node.FunctionNode;
import com.github.bohnman.squiggly.core.parse.ParseContext;
import com.github.bohnman.squiggly.core.variable.MapVariableResolver;
import com.github.bohnman.squiggly.core.variable.SquigglyVariableResolver;
import org.junit.Test;

import javax.annotation.Nullable;
import java.util.function.Function;
import java.util.function.Predicate;

import static org.junit.Assert.assertEquals;

public class SquigglyFunctionInvokerTest {

    private final SquigglyVariableResolver variableResolver = new MapVariableResolver();
    private final SquigglyConfig config = new SquigglyConfig();
    private final SquigglyConverterRegistry converterRegistry = SquigglyConverterRegistries.create(DefaultConverters::add);
    private final SquigglyConversionService conversionService = new DefaultConversionService(config, converterRegistry);
    private final ParseContext parseContext = new ParseContext(1, 1);


//    @Test
//    public void testInvoke() {
//        SquigglyFunctionInvoker invoker = invoker(Functions.class);
//        CoreAssert.throwsAny(() -> invoker.invoke("blah", function("blah")), SquigglyParseException.class);
//        assertEquals(Result.FOO_NO_ARGS, invoker.invoke(null, function("foo")));
//        assertEquals(Result.FOO_OBJECT_ARG, invoker.invoke(null, function("foo", input())));
//        assertEquals(Result.FOO_OBJECT_ARG, invoker.invoke(new Object(), function("foo", input())));
//        assertEquals(Result.FOO_NUMBER_ARG, invoker.invoke(1, function("foo", input())));
//        assertEquals(Result.FOO_LAMBDA_ARG, invoker.invoke(lambda(), function("foo", input())));
//        assertEquals(Result.FOO_FUNCTION_ARG, invoker.invoke(fn(), function("foo", input())));
//        assertEquals(Result.FOO_PREDICATE_ARG, invoker.invoke(predicate(), function("foo", input())));
//    }


    @Test
    public void testLambdaMatching() {
        SquigglyFunctionInvoker invoker = invoker(LambdaFunctions.class);
        assertEquals(Result.LAMBDA_FUNCTION_ARG, invoker.invoke(fn(), function("lambda", input())));
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(lambda(), function("lambda", input())));
        assertEquals(Result.LAMBDA_PREDICATE_ARG, invoker.invoke(predicate(), function("lambda", input())));
    }

    @Test
    public void testCoreLambdaIsTarget() {
        SquigglyFunctionInvoker invoker = invoker(LambdaTargetFunctions.class);
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(lambda(), function("lambda", input())));
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(null, function("lambda", input())));
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(new Object(), function("lambda", input())));
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(fn(), function("lambda", input())));
        assertEquals(Result.LAMBDA_LAMBDA_ARG, invoker.invoke(predicate(), function("lambda", input())));
    }

    private Predicate predicate() {
        return new TestPredicate();
    }

    private Function fn() {
        return new TestFunction();
    }

    private CoreLambda lambda() {
        return new TestCoreLambda();
    }


    private FunctionNode function(String name, ArgumentNode.Builder... args) {
        return functionBuilder(name, args).build();
    }

    private FunctionNode.Builder functionBuilder(String name, ArgumentNode.Builder... args) {
        FunctionNode.Builder builder = FunctionNode.builder()
                .name(name)
                .context(parseContext);

        for (ArgumentNode.Builder arg : args) {
            builder.argument(arg);
        }

        return builder;
    }

    private ArgumentNode.Builder input() {
        return arg(ArgumentNodeType.INPUT, ArgumentNodeType.INPUT);
    }


    private ArgumentNode.Builder arg(ArgumentNodeType type, Object value) {
        return ArgumentNode.builder()
                .context(parseContext)
                .type(type)
                .value(value);
    }


    private SquigglyFunctionInvoker invoker(Object... owners) {
        SquigglyFunctionRepository functionRepository = new MapFunctionRepository(SquigglyFunctions.create(owners));
        return new SquigglyFunctionInvoker(config, conversionService, functionRepository, variableResolver);
    }


    enum Result {
        FOO_FUNCTION_ARG,
        FOO_LAMBDA_ARG,
        FOO_NUMBER_ARG,
        FOO_OBJECT_ARG,
        FOO_PREDICATE_ARG,
        FOO_NO_ARGS,

        LAMBDA_FUNCTION_ARG,
        LAMBDA_LAMBDA_ARG,
        LAMBDA_PREDICATE_ARG
    }

    public static class Functions {

        public static Result foo() {
            return Result.FOO_NO_ARGS;
        }

        public static Result foo(Object o) {
            return Result.FOO_OBJECT_ARG;
        }

        public static Result foo(Number o) {
            return Result.FOO_NUMBER_ARG;
        }

        public static Result foo(CoreLambda o) {
            return Result.FOO_LAMBDA_ARG;
        }

        public static Result foo(Function o) {
            return Result.FOO_FUNCTION_ARG;
        }

        public static Result foo(Predicate p) {
            return Result.FOO_PREDICATE_ARG;
        }

    }

    private static class TestCoreLambda implements CoreLambda {
        @Nullable
        @Override
        public Object invoke(Object... objects) {
            return null;
        }
    }

    private static class TestFunction implements Function {
        @Override
        public Object apply(Object o) {
            return null;
        }
    }

    private static class TestPredicate implements Predicate {
        @Override
        public boolean test(Object o) {
            return false;
        }
    }

    private static class LambdaFunctions {
        public static Result lambda(CoreLambda o) {
            return Result.LAMBDA_LAMBDA_ARG;
        }

        public static Result lambda(Function o) {
            return Result.LAMBDA_FUNCTION_ARG;
        }

        public static Result lambda(Predicate o) {
            return Result.LAMBDA_PREDICATE_ARG;
        }
    }

    private static class LambdaTargetFunctions {
        public static Result lambda(CoreLambda o) {
            return Result.LAMBDA_LAMBDA_ARG;
        }

    }


}
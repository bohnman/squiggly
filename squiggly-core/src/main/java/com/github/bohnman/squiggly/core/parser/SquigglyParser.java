package com.github.bohnman.squiggly.core.parser;

import com.github.bohnman.core.antlr4.ThrowingErrorListener;
import com.github.bohnman.core.cache.Cache;
import com.github.bohnman.core.cache.CacheBuilder;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.config.SystemFunctionName;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.core.metric.source.CoreCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.core.name.AnyDeepName;
import com.github.bohnman.squiggly.core.name.AnyShallowName;
import com.github.bohnman.squiggly.core.name.ExactName;
import com.github.bohnman.squiggly.core.name.RegexName;
import com.github.bohnman.squiggly.core.name.SquigglyName;
import com.github.bohnman.squiggly.core.name.VariableName;
import com.github.bohnman.squiggly.core.name.WildcardName;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionBaseVisitor;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionLexer;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionParser;
import com.github.bohnman.squiggly.core.view.PropertyView;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.util.stream.Collectors.toList;

/**
 * The parser takes a filter expression and compiles it to an Abstract Syntax Tree (AST).  In this parser's case, the
 * tree doesn't have a root node but rather just returns top level nodes.
 */
@ThreadSafe
public class SquigglyParser {

    public static final String OP_AT_BRACKET_LEFT_SAFE = "@?[";
    public static final String OP_BRACKET_LEFT_SAFE = "?[";
    public static final String OP_SAFE_NAVIGATION = "?.";
    public static final String OP_AT_DOT_SAFE = "@?.";
    public static final String OP_AT = "@";

    // Caches parsed filter expressions
    private final Cache<String, List<SquigglyNode>> cache;

    public SquigglyParser(SquigglyConfig config, SquigglyMetrics metrics) {
        cache = CacheBuilder.from(config.getParserNodeCacheSpec()).build();
        metrics.add(new CoreCacheSquigglyMetricsSource("squiggly.parser.nodeCache.", cache));
    }

    /**
     * Parse a filter expression.
     *
     * @param filter the filter expression
     * @return compiled nodes
     */
    public List<SquigglyNode> parse(String filter) {
        filter = CoreStrings.trim(filter);

        if (CoreStrings.isEmpty(filter)) {
            return Collections.emptyList();
        }

        // get it from the cache if we can
        List<SquigglyNode> cachedNodes = cache.get(filter);

        if (cachedNodes != null) {
            return cachedNodes;
        }


        SquigglyExpressionLexer lexer = ThrowingErrorListener.overwrite(new SquigglyExpressionLexer(CharStreams.fromString(filter)));
        SquigglyExpressionParser parser = ThrowingErrorListener.overwrite(new SquigglyExpressionParser(new CommonTokenStream(lexer)));

        Visitor visitor = new Visitor();
        List<SquigglyNode> nodes = Collections.unmodifiableList(visitor.visit(parser.parse()));

        cache.put(filter, nodes);
        return nodes;
    }

    private class Visitor extends SquigglyExpressionBaseVisitor<List<SquigglyNode>> {
        @Override
        public List<SquigglyNode> visitParse(SquigglyExpressionParser.ParseContext ctx) {
            MutableNode root = new MutableNode(parseContext(ctx), new ExactName("root")).dotPathed(true);
            handleExpressionList(ctx.expressionList(), root);
            MutableNode analyzedRoot = analyze(root);
            return analyzedRoot.toSquigglyNode().getChildren();
        }

        private ParseContext parseContext(ParserRuleContext ctx) {
            Token start = ctx.getStart();
            return new ParseContext(start.getLine(), start.getCharPositionInLine());
        }

        private void handleExpressionList(SquigglyExpressionParser.ExpressionListContext ctx, MutableNode parent) {
            List<SquigglyExpressionParser.ExpressionContext> expressions = ctx.expression();

            for (SquigglyExpressionParser.ExpressionContext expressionContext : expressions) {
                handleExpression(expressionContext, parent);
            }
        }

        private void handleExpression(SquigglyExpressionParser.ExpressionContext ctx, MutableNode parent) {

            if (ctx.negatedExpression() != null) {
                handleNegatedExpression(ctx.negatedExpression(), parent);
            }

            List<SquigglyName> names;
            List<ParserRuleContext> ruleContexts;
            List<SquigglyExpressionParser.DottedFieldContext> dottedFields = ctx.dottedField();

            if (dottedFields.size() > 0) {
                SquigglyExpressionParser.DottedFieldContext dottedField = dottedFields.get(0);
                parent = handleDottedField(dottedField, parent);
                SquigglyExpressionParser.FieldContext lastField = dottedField.field().get(dottedField.field().size() - 1);
                names = Collections.singletonList(createName(lastField));
                ruleContexts = Collections.singletonList(lastField);
            } else if (ctx.fieldGroup() != null) {
                names = new ArrayList<>(ctx.fieldGroup().field().size());
                ruleContexts = new ArrayList<>(ctx.fieldGroup().field().size());
                for (SquigglyExpressionParser.FieldContext fieldContext : ctx.fieldGroup().field()) {
                    names.add(createName(fieldContext));
                    ruleContexts.add(fieldContext);
                }
            } else if (ctx.wildcardDeepField() != null) {
                names = Collections.singletonList(AnyDeepName.get());
                ruleContexts = Collections.singletonList(ctx.wildcardDeepField());
            } else {
                ruleContexts = Collections.singletonList(ctx);
                names = Collections.emptyList();
            }

            List<FunctionNode> keyFunctions = Collections.emptyList();
            List<FunctionNode> valueFunctions = Collections.emptyList();

            if (ctx.keyValueFieldArgChain() != null) {
                keyFunctions = parseKeyFunctionChain(ctx.keyValueFieldArgChain());
                valueFunctions = parseValueFunctionChain(ctx.keyValueFieldArgChain());
            }


            for (int i = 0; i < names.size(); i++) {
                SquigglyName name = names.get(i);
                ParserRuleContext ruleContext = ruleContexts.get(i);
                MutableNode node = parent.addChild(new MutableNode(parseContext(ruleContext), name));
                node.keyFunctions(keyFunctions);
                node.valueFunctions(valueFunctions);

                if (ctx.nestedExpression() != null) {
                    if (ctx.nestedExpression().expressionList() == null) {
                        node.emptyNested = true;
                    } else {
                        node.squiggly = true;
                        handleExpressionList(ctx.nestedExpression().expressionList(), node);
                    }
                } else if (dottedFields.size() > 1) {
                    SquigglyExpressionParser.DottedFieldContext dottedField = dottedFields.get(1);
                    node = handleDottedField(dottedField, node);
                    SquigglyExpressionParser.FieldContext lastField = dottedField.field().get(dottedField.field().size() - 1);
                    node.addChild(new MutableNode(parseContext(lastField), createName(lastField)));
                }
            }
        }

        private MutableNode handleDottedField(SquigglyExpressionParser.DottedFieldContext dottedField, MutableNode parent) {
            parent.squiggly = dottedField.field().size() > 1;
            for (int i = 0; i < dottedField.field().size() - 1; i++) {
                SquigglyExpressionParser.FieldContext field = dottedField.field(i);
                parent = parent.addChild(new MutableNode(parseContext(field), createName(field)).dotPathed(true));
                parent.squiggly = true;
            }
            return parent;
        }

        private List<FunctionNode> parseKeyFunctionChain(SquigglyExpressionParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty()) {
                return Collections.emptyList();
            }

            if (!context.fieldArgChain().isEmpty()) {
                return parseFieldArgChain(context.fieldArgChain().get(0));
            }

            if (!context.assignment().isEmpty()) {
                return parseAssignment(context.assignment().get(0));
            }

            return Collections.emptyList();
        }

        private List<FunctionNode> parseValueFunctionChain(SquigglyExpressionParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty() && context.fieldArgChain().size() == 1) {
                return parseFieldArgChain(context.fieldArgChain(0));
            }

            if (context.fieldArgChain().size() == 2) {
                return parseFieldArgChain(context.fieldArgChain(1));
            }

            if (context.Colon().isEmpty() && context.assignment().size() == 1) {
                return parseAssignment(context.assignment(0));
            }

            if (context.assignment().size() == 2) {
                return parseAssignment(context.assignment(1));
            }

            if (context.continuingFieldArgChain() != null) {
                return parseContinuingFieldArgChain(context.continuingFieldArgChain());
            }

            return Collections.emptyList();
        }

        private List<FunctionNode> parseAssignment(SquigglyExpressionParser.AssignmentContext context) {
            return Collections.singletonList(FunctionNode.builder()
                    .context(parseContext(context))
                    .name(SystemFunctionName.ASSIGN.getFunctionName())
                    .parameter(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                    .parameter(buildArg(context.arg()))
                    .build());

        }

        private List<FunctionNode> parseFieldArgChain(SquigglyExpressionParser.FieldArgChainContext context) {
            int size = 1;

            if (context.continuingFieldArgChain() != null) {
                size += context.continuingFieldArgChain().continuingFieldArgChainLink().size();
            }

            List<FunctionNode> functionNodes = new ArrayList<>(size);

            if (context.standaloneFieldArg() != null) {
                functionNodes.add(buildStandaloneFieldArg(context.standaloneFieldArg(), null));
            }

            if (context.function() != null) {
                functionNodes.add(buildFunction(context.function(), null, true).build());
            }

            if (context.continuingFieldArgChain() != null) {
                parseContinuingFieldArgChain(context.continuingFieldArgChain(), functionNodes);
            }

            return functionNodes;
        }

        private List<FunctionNode> parseContinuingFieldArgChain(SquigglyExpressionParser.ContinuingFieldArgChainContext context) {
            List<FunctionNode> functionNodes = new ArrayList<>(context.continuingFieldArgChainLink().size());
            parseContinuingFieldArgChain(context, functionNodes);
            return functionNodes;
        }

        private void parseContinuingFieldArgChain(SquigglyExpressionParser.ContinuingFieldArgChainContext context, List<FunctionNode> functionNodes) {
            for (SquigglyExpressionParser.ContinuingFieldArgChainLinkContext linkContext : context.continuingFieldArgChainLink()) {
                functionNodes.add(parseContinuingFieldArgChainLink(linkContext));

            }
        }

        private FunctionNode parseContinuingFieldArgChainLink(SquigglyExpressionParser.ContinuingFieldArgChainLinkContext context) {
            SquigglyExpressionParser.AccessOperatorContext opContext = context.accessOperator();

            if (context.function() != null) {
                return buildFunction(context.function(), opContext, true).build();
            }

            if (context.standaloneFieldArg() != null) {
                return buildStandaloneFieldArg(context.standaloneFieldArg(), opContext);
            }

            throw new IllegalStateException(format("%s: unknown field arg chain link [%s]", parseContext(context), context.getText()));
        }

        private FunctionNode buildStandaloneFieldArg(SquigglyExpressionParser.StandaloneFieldArgContext context, SquigglyExpressionParser.AccessOperatorContext opContext) {
            if (context.intRange() != null) {
                ArgumentNode.Builder arg = buildIntRange(context.intRange());
                return FunctionNode.builder()
                        .context(parseContext(context))
                        .name(SystemFunctionName.SLICE.getFunctionName())
                        .parameter(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                        .parameter(arg)
                        .build();
            }

            if (context.arrayAccessor() != null) {
                String integer = CoreStrings.substring(context.arrayAccessor().getText(), 1, -1);
                ArgumentNode.Builder arg = baseArg(context, ArgumentNodeType.INTEGER).value(Integer.parseInt(integer));

                return FunctionNode.builder()
                        .context(parseContext(context))
                        .name(SystemFunctionName.GET.getFunctionName())
                        .parameter(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                        .parameter(arg)
                        .build();
            }

            throw new IllegalStateException(format("%s: unknown standalone field arg [%s]", parseContext(context), context.getText()));
        }

        private FunctionNode.Builder buildFunction(SquigglyExpressionParser.ArgChainLinkContext context, boolean input) {
            if (context.functionAccessor() != null) {
                return buildFunction(context.functionAccessor().function(), context.functionAccessor().accessOperator(), input);
            }

            if (context.propertyAccessor() != null) {
                return buildPropertyFunction(context.propertyAccessor(), input);
            }

            throw new IllegalStateException(format("%s: unknown arg chain link [%s]", parseContext(context), context.getText()));
        }

        @SuppressWarnings("SameParameterValue")
        private FunctionNode parseFunction(SquigglyExpressionParser.ArgChainLinkContext context, boolean input) {
            return buildFunction(context, input).build();
        }

        private FunctionNode.Builder buildPropertyFunction(SquigglyExpressionParser.PropertyAccessorContext context, boolean input) {
            Object value;
            ArgumentNodeType type;

            if (context.Identifier() != null) {
                value = context.Identifier().getText();
                type = ArgumentNodeType.STRING;
            } else if (context.StringLiteral() != null) {
                value = unescapeString(context.StringLiteral().getText());
                type = ArgumentNodeType.STRING;
            } else if (context.variable() != null) {
                value = buildVariableValue(context.variable());
                type = ArgumentNodeType.VARIABLE;
            } else {
                throw new IllegalStateException(format("%s: Cannot find property name [%s]", parseContext(context), context.getText()));
            }

            String op = null;

            if (context.accessOperator() != null) {
                op = context.accessOperator().getText();
            } else if (context.BracketLeftSafe() != null) {
                op = context.BracketLeft().getText();
            } else if (context.BracketLeft() != null) {
                op = context.BracketLeft().getText();
            }

            return buildBasePropertyFunction(context, op)
                    .parameter(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildPropertyFunction(SquigglyExpressionParser.InitialPropertyAccessorContext context) {
            Object value;
            ArgumentNodeType type;

            String op = null;

            if (context.At() != null) {
                op = context.At().getText();
            } else if (context.AtDotSafe() != null) {
                op = context.AtDotSafe().getText();
            } else if (context.AtDotSafe() != null) {
                op = context.AtDotSafe().getText();
            } else if (context.AtBrackLeft() != null) {
                op = context.AtBrackLeft().getText();
            } else if (context.AtBrackLeftSafe() != null) {
                op = context.AtBrackLeftSafe().getText();
            }

            if (context.Identifier() != null) {
                value = context.Identifier().getText();
                type = ArgumentNodeType.STRING;
            } else if (context.StringLiteral() != null) {
                value = unescapeString(context.StringLiteral().getText());
                type = ArgumentNodeType.STRING;
            } else if (context.variable() != null) {
                value = buildVariableValue(context.variable());
                type = ArgumentNodeType.VARIABLE;
            } else if (context.function() != null || OP_AT.equals(op)) {
                value = "@";
                type = ArgumentNodeType.STRING;
            } else {
                throw new IllegalStateException(format("%s: Cannot find initial property name [%s]", parseContext(context), context.getText()));
            }

            return buildBasePropertyFunction(context, op)
                    .parameter(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildBasePropertyFunction(ParserRuleContext context, String operator) {
            FunctionNode.Builder function = FunctionNode.builder()
                    .context(parseContext(context))
                    .name(SystemFunctionName.PROPERTY.getFunctionName())
                    .type(FunctionNodeType.PROPERTY)
                    .parameter(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT));

            if (OP_SAFE_NAVIGATION.equals(operator)
                    || OP_AT_BRACKET_LEFT_SAFE.equals(operator)
                    || OP_AT_DOT_SAFE.equals(operator)
                    || OP_BRACKET_LEFT_SAFE.equals(operator)) {
                function.ignoreNulls(true);
            }

            return function;
        }

        private FunctionNode.Builder buildFunction(SquigglyExpressionParser.FunctionContext functionContext, @Nullable SquigglyExpressionParser.AccessOperatorContext operatorContext, boolean input) {
            ParseContext context = parseContext(functionContext);
            FunctionNode.Builder builder = buildBaseFunction(functionContext, context);

            if (input) {
                builder.parameter(baseArg(functionContext, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT));
            }

            applyParameters(builder, functionContext);

            if (operatorContext != null && OP_SAFE_NAVIGATION.equals(operatorContext.getText())) {
                builder.ignoreNulls(true);
            }

            return builder;
        }


        private FunctionNode.Builder buildBaseFunction(SquigglyExpressionParser.FunctionContext functionContext, ParseContext context) {
            return FunctionNode.builder()
                    .context(context)
                    .name(functionContext.functionName().getText());
        }

        private void applyParameters(FunctionNode.Builder builder, SquigglyExpressionParser.FunctionContext functionContext) {
            SquigglyExpressionParser.FunctionParametersContext functionParametersContext = functionContext.functionParameters();

            if (functionParametersContext != null) {
                functionParametersContext.functionParameter().forEach(parameter -> applyParameter(builder, parameter));
            }
        }

        private void applyParameter(FunctionNode.Builder builder, SquigglyExpressionParser.FunctionParameterContext parameter) {
            ArgumentNode.Builder arg = buildArg(parameter.arg());
            builder.parameter(arg);
        }

        private ArgumentNode.Builder buildArg(SquigglyExpressionParser.ArgContext arg) {
            Object value;
            ArgumentNodeType type;

            if (arg.argChain() != null) {
                return buildArgChain(arg.argChain());
            }

            if (arg.lambda() != null) {
                return buildLambda(arg.lambda());
            }

            if (arg.arg() != null && !arg.arg().isEmpty()) {
                return buildSubArg(arg);
            }


            throw new IllegalStateException(format("%s: Unknown arg type [%s]", parseContext(arg), arg.getText()));
        }

        private ArgumentNode.Builder buildLambda(SquigglyExpressionParser.LambdaContext lambda) {
            List<String> arguments = lambda.lambdaArg()
                    .stream()
                    .map(arg -> arg.variable() == null ? "_" : buildVariableValue(arg.variable()))
                    .collect(toList());

            ParseContext parseContext = parseContext(lambda);
            FunctionNode body = FunctionNode.builder()
                    .name(SystemFunctionName.IDENTITY.getFunctionName())
                    .context(parseContext)
                    .parameter(buildArg(lambda.lambdaBody().arg()))
                    .build();


            LambdaNode lambdaNode = new LambdaNode(parseContext, arguments, body);

            return baseArg(lambda, ArgumentNodeType.LAMBDA)
                    .value(lambdaNode);
        }

        private ArgumentNode.Builder buildSubArg(SquigglyExpressionParser.ArgContext arg) {
            if (arg.argGroupStart() != null) {
                ArgumentNode.Builder groupArg = buildArg(arg.arg(0));

                if (arg.argChainLink() != null) {
                    List<FunctionNode> functionNodes = new ArrayList<>(arg.argChainLink().size() + 1);

                    functionNodes.add(FunctionNode.builder()
                            .context(parseContext(arg))
                            .name(SystemFunctionName.IDENTITY.getFunctionName())
                            .parameter(groupArg)
                            .build()
                    );

                    for (SquigglyExpressionParser.ArgChainLinkContext linkContext : arg.argChainLink()) {
                        functionNodes.add(buildFunction(linkContext, true).build());
                    }


                    groupArg = baseArg(arg, ArgumentNodeType.FUNCTION_CHAIN).value(functionNodes);
                }

                return groupArg;

            }

            return buildArgExpression(arg);
        }

        private ArgumentNode.Builder buildArgExpression(SquigglyExpressionParser.ArgContext arg) {
            String op = getOp(arg);

            ParseContext parseContext = parseContext(arg);

            FunctionNode.Builder functionNode = FunctionNode.builder()
                    .context(parseContext)
                    .name(op);

            arg.arg().forEach(p -> functionNode.parameter(buildArg(p)));

            return baseArg(arg, ArgumentNodeType.FUNCTION_CHAIN)
                    .value(Collections.singletonList(functionNode.build()));
        }

        private String getOp(SquigglyExpressionParser.ArgContext arg) {
            if (matchOp(arg.Not(), arg.NotName())) {
                return SystemFunctionName.NOT.getFunctionName();
            }

            if (matchOp(arg.Add(), arg.AddName())) {
                return SystemFunctionName.ADD.getFunctionName();
            }

            if (matchOp(arg.Subtract(), arg.SubtractName())) {
                return SystemFunctionName.SUBTRACT.getFunctionName();
            }

            if (matchOp(arg.WildcardShallow(), arg.MultiplyName())) {
                return SystemFunctionName.MULTIPLY.getFunctionName();
            }

            if (matchOp(arg.SlashForward(), arg.DivideName())) {
                return SystemFunctionName.DIVIDE.getFunctionName();
            }

            if (matchOp(arg.Modulus(), arg.ModulusName())) {
                return SystemFunctionName.MODULUS.getFunctionName();
            }

            if (matchOp(arg.EqualsEquals(), arg.EqualsName())) {
                return SystemFunctionName.EQUALS.getFunctionName();
            }

            if (matchOp(arg.EqualsNot(), arg.EqualsNotName(), arg.EqualsNotSql())) {
                return SystemFunctionName.NOT_EQUALS.getFunctionName();
            }

            if (matchOp(arg.AngleLeft(), arg.LessThanName())) {
                return SystemFunctionName.LESS_THAN.getFunctionName();
            }

            if (matchOp(arg.LessThanEquals(), arg.LessThanEqualsName())) {
                return SystemFunctionName.LESS_THAN_EQUALS.getFunctionName();
            }

            if (matchOp(arg.AngleRight(), arg.GreaterThanName())) {
                return SystemFunctionName.GREATER_THAN.getFunctionName();
            }

            if (matchOp(arg.GreaterThanEquals(), arg.GreaterThanEqualsName())) {
                return SystemFunctionName.GREATER_THAN_EQUALS.getFunctionName();
            }

            if (matchOp(arg.Match(), arg.MatchName())) {
                return SystemFunctionName.MATCH.getFunctionName();
            }

            if (matchOp(arg.MatchNot(), arg.MatchNotName())) {
                return SystemFunctionName.NOT_MATCH.getFunctionName();
            }

            if (matchOp(arg.Or(), arg.OrName())) {
                return SystemFunctionName.OR.getFunctionName();
            }

            if (matchOp(arg.And(), arg.AndName())) {
                return SystemFunctionName.AND.getFunctionName();
            }

            throw new IllegalStateException(format("%s: unknown op [%s]", parseContext(arg), arg.getText()));
        }

        private boolean matchOp(TerminalNode token1, TerminalNode token2) {
            return token1 != null || token2 != null;
        }

        private boolean matchOp(TerminalNode token1, TerminalNode token2, TerminalNode token3) {
            return token1 != null || token2 != null || token3 != null;
        }

        private ArgumentNode.Builder buildLiteral(SquigglyExpressionParser.LiteralContext context) {
            if (context.BooleanLiteral() != null) {
                return buildBoolean(context);
            }


            if (context.FloatLiteral() != null) {
                return buildFloat(context);
            }

            if (context.IntegerLiteral() != null) {
                return buildInteger(context);
            }

            if (context.RegexLiteral() != null) {
                return buildRegex(context);
            }

            if (context.StringLiteral() != null) {
                return buildString(context);
            }

            throw new IllegalStateException(format("%s: Unknown literal type [%s]", parseContext(context), context.getText()));
        }

        private ArgumentNode.Builder buildIntRange(SquigglyExpressionParser.IntRangeContext context) {
            List<SquigglyExpressionParser.IntRangeArgContext> intRangeArgs;
            boolean exclusiveEnd;

            if (context.inclusiveExclusiveIntRange() != null) {
                intRangeArgs = context.inclusiveExclusiveIntRange().intRangeArg();
                exclusiveEnd = true;
            } else if (context.inclusiveInclusiveIntRange() != null) {
                intRangeArgs = context.inclusiveInclusiveIntRange().intRangeArg();
                exclusiveEnd = false;
            } else {
                throw new IllegalStateException(format("%s: Unknown int range type [%s]", parseContext(context), context.getText()));
            }

            ArgumentNode.Builder start = null;
            ArgumentNode.Builder end = null;

            if (intRangeArgs.isEmpty()) {
                start = baseArg(context, ArgumentNodeType.INTEGER).value(0);
            }

            if (intRangeArgs.size() > 0) {
                start = buildIntRangeArg(intRangeArgs.get(0));
            }

            if (intRangeArgs.size() > 1) {
                end = buildIntRangeArg(intRangeArgs.get(1));
            }

            return baseArg(context, ArgumentNodeType.INT_RANGE)
                    .value(new IntRangeNode(start, end, exclusiveEnd));
        }

        private ArgumentNode.Builder buildIntRangeArg(SquigglyExpressionParser.IntRangeArgContext context) {
            if (context.variable() != null) {
                return buildVariable(context.variable());
            }

            if (context.IntegerLiteral() != null) {
                return buildInteger(context);
            }


            throw new IllegalStateException(format("%s: Unknown int range arg type [%s]", parseContext(context), context.getText()));
        }

        private ArgumentNode.Builder baseArg(ParserRuleContext context, ArgumentNodeType type) {
            ParseContext parseContext = parseContext(context);
            return ArgumentNode.builder().context(parseContext).type(type);
        }

        private ArgumentNode.Builder buildBoolean(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.BOOLEAN)
                    .value("true".equalsIgnoreCase(context.getText()));
        }

        private ArgumentNode.Builder buildFloat(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.FLOAT).value(Float.parseFloat(context.getText()));
        }

        private ArgumentNode.Builder buildArgChain(SquigglyExpressionParser.ArgChainContext context) {
            int functionLength = (context.argChainLink() == null) ? 0 : context.argChainLink().size();
            functionLength += 2;
            List<FunctionNode> functionNodes = new ArrayList<>(functionLength);

            if (context.literal() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.literal()))
                        .name(SystemFunctionName.IDENTITY.getFunctionName())
                        .parameter(buildLiteral(context.literal()))
                        .build()
                );
            }

            if (context.intRange() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.intRange()))
                        .name(SystemFunctionName.IDENTITY.getFunctionName())
                        .parameter(buildIntRange(context.intRange()))
                        .build()
                );
            }

            if (context.variable() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.variable()))
                        .name(SystemFunctionName.IDENTITY.getFunctionName())
                        .parameter(buildVariable(context.variable()))
                        .build()
                );
            }

            boolean ascending = true;

            if (context.propertySortDirection() != null) {
                ascending = !"-".equals(context.propertySortDirection().getText());
            }

            if (context.initialPropertyAccessor() != null) {
                functionNodes.add(buildPropertyFunction(context.initialPropertyAccessor()).ascending(ascending).build());

                if (context.initialPropertyAccessor().function() != null) {
                    functionNodes.add(buildFunction(context.initialPropertyAccessor().function(), null, true)
                            .ignoreNulls(context.initialPropertyAccessor().AtDotSafe() != null)
                            .build());
                }
            }

            if (context.function() != null) {
                boolean input = !functionNodes.isEmpty();
                functionNodes.add(buildFunction(context.function(), null, input).ascending(ascending).build());
            }

            if (context.argChainLink() != null) {
                boolean input = !functionNodes.isEmpty();

                for (SquigglyExpressionParser.ArgChainLinkContext linkContext : context.argChainLink()) {
                    functionNodes.add(buildFunction(linkContext, input).ascending(ascending).build());
                }
            }

            return baseArg(context, ArgumentNodeType.FUNCTION_CHAIN)
                    .value(functionNodes);
        }

        private ArgumentNode.Builder buildInteger(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.INTEGER).value(Integer.parseInt(context.getText()));
        }


        private ArgumentNode.Builder buildRegex(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.REGEX).value(buildPattern(context.getText()));
        }

        private ArgumentNode.Builder buildString(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.STRING).value(unescapeString(context.getText()));
        }

        private String unescapeString(String text) {
            if (text == null) {
                return null;
            }

            if (text.length() < 2) {
                return text;
            }

            if (text.startsWith("'") && text.endsWith("'")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            }

            if (text.startsWith("\"") && text.endsWith("\"")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            }

            return text;
        }

        private ArgumentNode.Builder buildVariable(SquigglyExpressionParser.VariableContext context) {
            return baseArg(context, ArgumentNodeType.VARIABLE).value(buildVariableValue(context));
        }

        private String buildVariableValue(SquigglyExpressionParser.VariableContext context) {
            return unescapeString(context.getText().substring(1));
        }

        private SquigglyName createName(SquigglyExpressionParser.FieldContext ctx) {
            SquigglyName name;

            if (ctx.StringLiteral() != null) {
                name = new ExactName(unescapeString(ctx.StringLiteral().getText()));
            } else if (ctx.namedOperator() != null) {
                name = new ExactName(ctx.namedOperator().getText());
            } else if (ctx.Identifier() != null) {
                name = new ExactName(ctx.Identifier().getText());
            } else if (ctx.wildcardField() != null) {
                name = new WildcardName(ctx.wildcardField().getText());
            } else if (ctx.RegexLiteral() != null) {


                Pattern pattern = buildPattern(ctx.RegexLiteral().getText());

                name = new RegexName(pattern.pattern(), pattern);
            } else if (ctx.wildcard() != null) {
                if ("*".equals(ctx.wildcard().getText())) {
                    name = AnyShallowName.get();
                } else {
                    name = new WildcardName(ctx.wildcard().getText());
                }
            } else if (ctx.variable() != null) {
                name = new VariableName(buildVariableValue(ctx.variable()));
            } else {
                throw new IllegalArgumentException("Unhandled field: " + ctx.getText());
            }

            return name;
        }

        private Pattern buildPattern(String fullPattern) {
            String pattern = fullPattern.substring(1);
            int slashIdx = pattern.indexOf('/');

            if (slashIdx < 0) {
                slashIdx = pattern.indexOf('~');
            }

            Set<String> flags = new HashSet<>();

            if (slashIdx >= 0) {
                String flagPart = CoreStrings.trim(CoreStrings.substring(pattern, slashIdx + 1));
                pattern = CoreStrings.substring(pattern, 0, slashIdx);

                if (CoreStrings.isNotEmpty(flagPart)) {
                    for (char flag : flagPart.toCharArray()) {
                        flags.add(Character.toString(flag));
                    }
                }
            }

            int flagMask = 0;

            if (flags != null && !flags.isEmpty()) {
                for (String flag : flags) {
                    switch (flag) {
                        case "i":
                            flagMask |= Pattern.CASE_INSENSITIVE;
                            break;
                        default:
                            throw new IllegalArgumentException("Unrecognized flag " + flag + " for pattern " + pattern);
                    }
                }
            }

            return Pattern.compile(pattern, flagMask);
        }


        private void handleNegatedExpression(SquigglyExpressionParser.NegatedExpressionContext ctx, MutableNode parent) {
            if (ctx.field() != null) {
                parent.addChild(new MutableNode(parseContext(ctx.field()), createName(ctx.field())).negated(true));
            } else if (ctx.dottedField() != null) {
                for (SquigglyExpressionParser.FieldContext fieldContext : ctx.dottedField().field()) {
                    parent.squiggly = true;
                    parent = parent.addChild(new MutableNode(parseContext(ctx.dottedField()), createName(fieldContext)).dotPathed(true));
                }

                parent.negated(true);
            }
        }

    }

    private MutableNode analyze(MutableNode node) {
        if (node.children != null && !node.children.isEmpty()) {
            boolean allNegated = true;

            for (MutableNode child : node.children.values()) {
                if (!child.negated) {
                    allNegated = false;
                    break;
                }
            }

            if (allNegated) {
                node.addChild(new MutableNode(node.getContext(), newBaseViewName()).dotPathed(node.dotPathed));
                MutableNode parent = node.parent;

                while (parent != null) {
                    parent.addChild(new MutableNode(parent.getContext(), newBaseViewName()).dotPathed(parent.dotPathed));

                    if (!parent.dotPathed) {
                        break;
                    }

                    parent = parent.parent;
                }
            } else {
                for (MutableNode child : node.children.values()) {
                    analyze(child);
                }
            }

        }

        return node;
    }

    private class MutableNode {
        private final ParseContext context;
        private SquigglyName name;
        private boolean negated;
        private boolean squiggly;
        private boolean emptyNested;
        @Nullable
        private Map<String, MutableNode> children;
        private boolean dotPathed;
        @Nullable
        private MutableNode parent;
        private List<FunctionNode> keyFunctions = new ArrayList<>();
        private List<FunctionNode> valueFunctions = new ArrayList<>();

        MutableNode(ParseContext context, SquigglyName name) {
            this.context = context;
            this.name = name;
        }

        SquigglyNode toSquigglyNode() {
            if (name == null) {
                throw new IllegalArgumentException("No Names specified");
            }

            List<SquigglyNode> childNodes;

            if (children == null || children.isEmpty()) {
                childNodes = Collections.emptyList();
            } else {
                childNodes = new ArrayList<>(children.size());

                for (MutableNode child : children.values()) {
                    childNodes.add(child.toSquigglyNode());
                }
            }

            return new SquigglyNode(context, name, childNodes, keyFunctions, valueFunctions, negated, squiggly, emptyNested);
        }

        public ParseContext getContext() {
            return context;
        }

        public MutableNode dotPathed(boolean dotPathed) {
            this.dotPathed = dotPathed;
            return this;
        }

        @SuppressWarnings("UnusedReturnValue")
        public MutableNode keyFunctions(List<FunctionNode> functions) {
            functions.forEach(this::keyFunction);
            return this;
        }

        public MutableNode keyFunction(FunctionNode function) {
            keyFunctions.add(function);
            return this;
        }

        @SuppressWarnings("UnusedReturnValue")
        public MutableNode valueFunctions(List<FunctionNode> functions) {
            functions.forEach(this::valueFunction);
            return this;
        }

        public MutableNode valueFunction(FunctionNode function) {
            valueFunctions.add(function);
            return this;
        }

        public MutableNode negated(boolean negated) {
            this.negated = negated;
            return this;
        }

        public MutableNode addChild(MutableNode childToAdd) {
            if (children == null) {
                children = new LinkedHashMap<>();
            }

            String name = childToAdd.name.getName();
            MutableNode existingChild = children.get(name);

            if (existingChild == null) {
                childToAdd.parent = this;
                children.put(name, childToAdd);
            } else {
                if (childToAdd.children != null) {

                    if (existingChild.children == null) {
                        existingChild.children = childToAdd.children;
                    } else {
                        existingChild.children.putAll(childToAdd.children);
                    }
                }


                existingChild.squiggly = existingChild.squiggly || childToAdd.squiggly;
                existingChild.emptyNested = existingChild.emptyNested && childToAdd.emptyNested;
                existingChild.dotPathed = existingChild.dotPathed && childToAdd.dotPathed;
                childToAdd = existingChild;
            }

            if (!childToAdd.dotPathed && dotPathed) {
                dotPathed = false;
            }

            return childToAdd;
        }

    }

    private ExactName newBaseViewName() {
        return new ExactName(PropertyView.BASE_VIEW);
    }


}

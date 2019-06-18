package com.github.bohnman.squiggly.parse;

import com.github.bohnman.core.antlr4.ThrowingErrorListener;
import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.environment.SquigglyEnvironmentOld;
import com.github.bohnman.squiggly.function.SystemFunctionName;
import com.github.bohnman.squiggly.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.metric.SquigglyMetricsProducer;
import com.github.bohnman.squiggly.metric.SquigglyMetricsTarget;
import com.github.bohnman.squiggly.name.SquigglyName;
import com.github.bohnman.squiggly.name.SquigglyNames;
import com.github.bohnman.squiggly.node.*;
import com.github.bohnman.squiggly.parse.antlr4.SquigglyGrammarBaseVisitor;
import com.github.bohnman.squiggly.parse.antlr4.SquigglyGrammarLexer;
import com.github.bohnman.squiggly.parse.antlr4.SquigglyGrammarParser;
import com.github.bohnman.squiggly.view.PropertyView;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

@ThreadSafe
public class AntlrSquigglyParser implements SquigglyParser, SquigglyMetricsProducer {

    // Caches parsed filter expressions
    private final CoreCache<String, FilterNode> cache;
    private final SquigglyMetricsProducer metrics;

    public AntlrSquigglyParser(SquigglyEnvironmentOld config, SquigglyMetrics metrics) {
        cache = CoreCacheBuilder.from(config.getParserNodeCacheSpec()).build();
        this.metrics = SquigglyMetrics.coreCacheProducer("squiggly.parser.node-cache.", cache);
    }

    @Override
    public void sendMetrics(SquigglyMetricsTarget target) {
        this.metrics.sendMetrics(target);
    }

    /**
     * Parse node filter expression.
     *
     * @param filter filter
     * @return list of squiggly nodes
     */
    @Override
    public FilterNode parseNodeFilter(String filter) {
        return parseFilter(filter, true);
    }

    /**
     * Parse a filter expression.
     *
     * @param filter the filter expression
     * @return compiled nodes
     */
    @Override
    public FilterNode parsePropertyFilter(String filter) {
        return parseFilter(filter, false);
    }

    private FilterNode parseFilter(String filter, boolean nodeFilter) {
        filter = CoreStrings.trim(filter);

        if (CoreStrings.isEmpty(filter)) {
            return FilterNode.EMPTY;
        }

        FilterNode filterNode = cache.get(filter);

        if (filterNode != null) {
            return filterNode;
        }

        SquigglyGrammarLexer lexer = ThrowingErrorListener.overwrite(new SquigglyGrammarLexer(CharStreams.fromString(filter)));
        SquigglyGrammarParser parser = ThrowingErrorListener.overwrite(new SquigglyGrammarParser(new CommonTokenStream(lexer)));
        FilterVisitor visitor = new FilterVisitor();

        filterNode = visitor.visit(nodeFilter ? parser.nodeFilter() : parser.propertyFilter());

        cache.put(filter, filterNode);
        return filterNode;
    }


    private class FilterVisitor extends SquigglyGrammarBaseVisitor<FilterNode> {

        @Override
        public FilterNode visitNodeFilter(SquigglyGrammarParser.NodeFilterContext ctx) {
            FilterNode.Builder filterNode = SquigglyNodes.filter(origin(ctx), ctx.nodeStatement().size());

            for (SquigglyGrammarParser.NodeStatementContext statementContext : ctx.nodeStatement()) {
                handleNodeStatement(statementContext, filterNode);
            }

            return analyze(filterNode).build();
        }

        private void handleNodeStatement(SquigglyGrammarParser.NodeStatementContext ctx, FilterNode.Builder filterNode) {
            SquigglyNodeOrigin origin = origin(ctx);
            StatementNode.Builder statementNode;

            if (ctx.expressionList() != null) {
                statementNode = SquigglyNodes.statement(origin);
                handleExpressionList(ctx.expressionList(), statementNode);
            } else if (ctx.topLevelExpression() != null) {
                statementNode = SquigglyNodes.statement(origin);
                handleTopLevelExpression(ctx.topLevelExpression(), statementNode);
            } else {
                throw new SquigglyParseException(origin, "Unknown node statement");
            }

            filterNode.statement(statementNode);
        }

        @Override
        public FilterNode visitPropertyFilter(SquigglyGrammarParser.PropertyFilterContext ctx) {
            FilterNode.Builder filterNode = SquigglyNodes.filter(origin(ctx), 1);
            handlePropertyStatement(ctx.propertyStatement(), filterNode);
            return analyze(filterNode).build();
        }

        private void handlePropertyStatement(SquigglyGrammarParser.PropertyStatementContext ctx, FilterNode.Builder filterNode) {
            SquigglyNodeOrigin origin = origin(ctx);
            StatementNode.Builder statementNode;

            if (ctx.expressionList() != null) {
                statementNode = SquigglyNodes.statement(origin);
                handleExpressionList(ctx.expressionList(), statementNode);
            } else {
                throw new SquigglyParseException(origin, "Unknown property statement");
            }

            filterNode.statement(statementNode);
        }

        private SquigglyNodeOrigin origin(ParserRuleContext ctx) {
            Token start = ctx.getStart();
            return SquigglyNodeOrigin.create(start.getLine(), start.getCharPositionInLine());
        }

        private void handleTopLevelExpression(SquigglyGrammarParser.TopLevelExpressionContext context, StatementNode.Builder statementNode) {
            ExpressionNode.Builder expressionNode = statementNode.getRoot();

            if (context.assignment() != null) {
                expressionNode.valueFunctions(parseAssignment(context.assignment()));
                return;
            }

            if (!context.argChainLink().isEmpty()) {
                expressionNode.valueFunctions(context.argChainLink()
                        .stream()
                        .map(argChainLink -> buildFunction(argChainLink, true).build())
                        .collect(Collectors.toList())
                );
            }
        }

        private void handleExpressionList(SquigglyGrammarParser.ExpressionListContext ctx, StatementNode.Builder statementNode) {
            List<SquigglyGrammarParser.ExpressionContext> expressions = ctx.expression();

            for (SquigglyGrammarParser.ExpressionContext expressionContext : expressions) {
                handleExpression(expressionContext, statementNode.getRoot());
            }
        }

        private void handleExpressionList(SquigglyGrammarParser.ExpressionListContext ctx, ExpressionNode.Builder parent) {
            List<SquigglyGrammarParser.ExpressionContext> expressions = ctx.expression();

            for (SquigglyGrammarParser.ExpressionContext expressionContext : expressions) {
                handleExpression(expressionContext, parent);
            }
        }

        private void handleExpression(SquigglyGrammarParser.ExpressionContext ctx, ExpressionNode.Builder parent) {

            if (ctx.negatedExpression() != null) {
                handleNegatedExpression(ctx.negatedExpression(), parent);
                return;
            }

            if (ctx.dottedFieldExpression() != null) {
                handleDottedFieldExpression(ctx.dottedFieldExpression(), parent);
                return;
            }

            if (ctx.fieldGroupExpression() != null) {
                handleFieldGroupExpression(ctx.fieldGroupExpression(), parent);
                return;
            }

            if (ctx.deepExpression() != null) {
                handleDeepExpression(ctx.deepExpression(), parent);
                return;
            }

            if (ctx.deepInheritExpression() != null) {
                handleDeepInheritExpression(ctx.deepInheritExpression(), parent);
                return;
            }

            throw new SquigglyParseException(origin(ctx), "Unhandled expression [%s].", ctx.getText());

        }


        private void handleDottedFieldExpression(SquigglyGrammarParser.DottedFieldExpressionContext ctx, ExpressionNode.Builder parent) {
            SquigglyGrammarParser.DottedFieldContext dottedField = ctx.dottedField();

            parent.nested(dottedField.field().size() > 1);

            for (int i = 0; i < dottedField.field().size() - 1; i++) {
                SquigglyGrammarParser.FieldContext field = dottedField.field(i);
                parent = expressionBuilder(field, createName(field), parent).dotPathed(true);
                parent.nested(true);
            }

            SquigglyGrammarParser.FieldContext lastField = ctx.dottedField().field(dottedField.field().size() - 1);
            expressionBuilder(lastField, createName(lastField), parent, ctx.nestedExpression(), ctx.keyValueFieldArgChain());
        }

        private void handleFieldGroupExpression(SquigglyGrammarParser.FieldGroupExpressionContext ctx, ExpressionNode.Builder parent) {
            SquigglyGrammarParser.FieldGroupContext fieldGroup = ctx.fieldGroup();

            for (SquigglyGrammarParser.FieldContext fieldContext : fieldGroup.field()) {
                expressionBuilder(fieldContext, createName(fieldContext), parent, ctx.nestedExpression(), ctx.keyValueFieldArgChain());
            }
        }

        private void handleDeepExpression(SquigglyGrammarParser.DeepExpressionContext ctx, ExpressionNode.Builder parent) {
            List<SquigglyGrammarParser.DeepArgContext> deepArgs = ctx.deepArg();

            Integer minDepth = null;
            Integer maxDepth = null;
            boolean maxDepthExclusive = false;
            SquigglyGrammarParser.DeepRangeContext deepRange = ctx.deepRange();

            if (deepRange != null && !deepRange.IntegerLiteral().isEmpty()) {
                SquigglyGrammarParser.IntRangeOpContext intRangeOp = deepRange.intRangeOp();
                maxDepthExclusive = intRangeOp.Colon() != null;

                if (deepRange.IntegerLiteral().size() > 1) {
                    minDepth = Integer.parseInt(deepRange.IntegerLiteral().get(0).getText());
                    maxDepth = Integer.parseInt(deepRange.IntegerLiteral().get(1).getText());

                } else {
                    TerminalNode integerLiteral = deepRange.IntegerLiteral().get(0);
                    int cmp = comparePositions(integerLiteral.getSymbol(), intRangeOp.getStart());

                    if (cmp < 0) {
                        minDepth = Integer.parseInt(integerLiteral.getText());
                    } else {
                        maxDepth = Integer.parseInt(integerLiteral.getText());
                    }
                }
            }

            if (maxDepth != null && !maxDepthExclusive) {
                maxDepth++;
            }

            if (minDepth != null && maxDepth != null && minDepth >= maxDepth) {
                throw new SquigglyParseException(origin(ctx), "Invalid deep range %s", ctx.getText());
            }

            if (deepArgs.isEmpty()) {
                ExpressionNode.Builder node = expressionBuilder(ctx, SquigglyNames.anyDeep(), parent)
                        .deep(true);

                if (minDepth != null) {
                    node.minDepth(node.getDepth() + minDepth);
                }

                if (maxDepth != null) {
                    node.maxDepth(node.getDepth() + maxDepth);
                }
                return;
            }

            for (SquigglyGrammarParser.DeepArgContext deepArg : deepArgs) {
                for (ExpressionNode.Builder node : handleDeepArg(deepArg, parent)) {
                    node.deep(true).minDepth(minDepth).maxDepth(maxDepth);
                }
            }
        }

        private void handleDeepInheritExpression(SquigglyGrammarParser.DeepInheritExpressionContext ctx, ExpressionNode.Builder parent) {
            expressionBuilder(ctx, SquigglyNames.deepInherit(), parent);
        }

        private List<ExpressionNode.Builder> handleDeepArg(SquigglyGrammarParser.DeepArgContext ctx, ExpressionNode.Builder parent) {
            if (ctx.Subtract() != null) {
                return Collections.singletonList(parent.child(SquigglyNodes.expression(origin(ctx.field())).name(createName(ctx.field())).negated(true)));
            }

            List<SquigglyGrammarParser.FieldContext> fields;
            ParserRuleContext ruleContext;

            if (ctx.field() != null) {
                fields = Collections.singletonList(ctx.field());
                ruleContext = ctx.field();
            } else if (ctx.fieldGroup() != null) {
                fields = ctx.fieldGroup().field();
                ruleContext = ctx.fieldGroup();
            } else {
                fields = Collections.emptyList();
                ruleContext = ctx;
            }

            return fields.stream()
                    .map(field -> expressionBuilder(ruleContext, createName(field), parent, null, ctx.keyValueFieldArgChain()))
                    .collect(toList());
        }


        private List<FunctionNode> parseKeyFunctionChain(SquigglyGrammarParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty()) {
                return Collections.emptyList();
            }

            return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(0));
        }

        private List<FunctionNode> parseValueFunctionChain(SquigglyGrammarParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty()) {
                return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(0));
            } else if (context.keyValueFieldArgChainLink().size() > 1) {
                return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(1));
            }

            return Collections.emptyList();
        }

        private List<FunctionNode> parseKeyValueFieldArgChainLink(SquigglyGrammarParser.KeyValueFieldArgChainLinkContext context) {
            if (context.assignment() != null) {
                return parseAssignment(context.assignment());
            }

            if (context.function() != null) {
                List<FunctionNode> functionNodes = new ArrayList<>(context.argChainLink().size() + 1);
                functionNodes.add(buildFunction(context.function(), null, true).build());

                for (SquigglyGrammarParser.ArgChainLinkContext linkContext : context.argChainLink()) {
                    functionNodes.add(buildFunction(linkContext, true).build());
                }

                return functionNodes;
            }

            throw new SquigglyParseException(origin(context), "Unhandled keyValueArgChainLink [%s]", context.getText());
        }

        private List<FunctionNode> parseAssignment(SquigglyGrammarParser.AssignmentContext context) {

            FunctionNode.Type type = FunctionNode.Type.ASSIGNMENT;

            String name = SystemFunctionName.ASSIGN.getFunctionName();

            return Collections.singletonList(SquigglyNodes.function(origin(context))
                    .name(name)
                    .type(type)
                    .argument(baseArg(context, ArgumentNode.Type.INPUT).value(ArgumentNode.Type.INPUT))
                    .argument(buildArg(context.arg(), context))
                    .build());

        }

        private FunctionNode.Builder buildFunction(SquigglyGrammarParser.ArgChainLinkContext context, boolean input) {
            if (context.functionAccessor() != null) {
                return buildFunction(context.functionAccessor().function(), context.functionAccessor().accessOperator(), input);
            }

            if (context.propertyAccessor() != null) {
                return buildPropertyFunction(context.propertyAccessor(), input);
            }

            throw new SquigglyParseException(origin(context), "unknown arg chain link [%s]", context.getText());
        }

        @SuppressWarnings("SameParameterValue")
        private FunctionNode parseFunction(SquigglyGrammarParser.ArgChainLinkContext context, boolean input) {
            return buildFunction(context, input).build();
        }

        private FunctionNode.Builder buildPropertyFunction(SquigglyGrammarParser.PropertyAccessorContext context, boolean input) {
            Object value;
            ArgumentNode.Type type;

            if (context.Identifier() != null) {
                value = context.Identifier().getText();
                type = ArgumentNode.Type.STRING;
            } else if (context.StringLiteral() != null) {
                value = unescapeString(context.StringLiteral().getText());
                type = ArgumentNode.Type.STRING;
            } else if (context.variable() != null) {
                value = buildVariableValue(context.variable());
                type = ArgumentNode.Type.VARIABLE;
            } else if (context.intRange() != null) {
                value = Collections.singletonList(buildIntRangeFunction(context.intRange()));
                type = ArgumentNode.Type.FUNCTION_CHAIN;
            } else {
                throw new SquigglyParseException(origin(context), "Cannot find property name [%s]", context.getText());
            }

            return buildBasePropertyFunction(context)
                    .argument(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildPropertyFunction(SquigglyGrammarParser.InitialPropertyAccessorContext context) {
            Object value;
            ArgumentNode.Type type;

            if (context.Identifier() != null) {
                value = context.Identifier().getText();
                type = ArgumentNode.Type.STRING;
            } else if (context.DollarDollar() != null) {
                value = context.DollarDollar().getText();
                type = ArgumentNode.Type.STRING;
            } else if (context.Dollar() != null) {
                value = context.Dollar().getText();
                type = ArgumentNode.Type.STRING;
            } else {
                throw new SquigglyParseException(origin(context), "Unknown initial property accessor [%s]", context.getText());
            }

            return buildBasePropertyFunction(context)
                    .initial(true)
                    .argument(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildBasePropertyFunction(ParserRuleContext context) {
            FunctionNode.Builder function = SquigglyNodes.function(origin(context))
                    .name(SystemFunctionName.PROPERTY.getFunctionName())
                    .type(FunctionNode.Type.PROPERTY)
                    .argument(baseArg(context, ArgumentNode.Type.INPUT).value(ArgumentNode.Type.INPUT))
                    .ignoreNulls(true);

            return function;
        }

        private FunctionNode.Builder buildFunction(SquigglyGrammarParser.FunctionContext functionContext, @Nullable SquigglyGrammarParser.AccessOperatorContext operatorContext, boolean input) {
            SquigglyNodeOrigin origin = origin(functionContext);
            FunctionNode.Builder builder = buildBaseFunction(functionContext, origin);

            if (input) {
                builder.argument(baseArg(functionContext, ArgumentNode.Type.INPUT).value(ArgumentNode.Type.INPUT));
            }

            applyParameters(builder, functionContext);

            return builder;
        }

        private FunctionNode.Builder buildBaseFunction(SquigglyGrammarParser.FunctionContext functionContext, SquigglyNodeOrigin origin) {
            String text = functionContext.functionName().getText();
            String name;

            if (text.length() == 1) {
                name = SystemFunctionName.PICK.getFunctionName();
            } else {
                name = text.substring(1).trim();
            }

            return SquigglyNodes.function(origin)
                    .name(name);
        }

        private void applyParameters(FunctionNode.Builder builder, SquigglyGrammarParser.FunctionContext functionContext) {
            functionContext.arg().forEach(parameter -> applyParameter(builder, parameter));
        }

        private void applyParameter(FunctionNode.Builder builder, SquigglyGrammarParser.ArgContext parameter) {
            ArgumentNode.Builder arg = buildArg(parameter);
            builder.argument(arg);
        }

        private ArgumentNode.Builder buildArg(SquigglyGrammarParser.ArgContext arg) {
            return buildArg(arg, arg);
        }

        private ArgumentNode.Builder buildArg(SquigglyGrammarParser.ArgContext arg, ParserRuleContext context) {
            Object value;
            ArgumentNode.Type type;

            if (arg == null) {
                return baseArg(context, ArgumentNode.Type.NULL).value(null);
            }

            if (arg.Null() != null) {
                return buildNull(arg);
            }

            if (arg.argChain() != null) {
                return buildArgChain(arg.argChain());
            }

            if (arg.lambda() != null) {
                return buildLambda(arg.lambda());
            }

            if (arg.arg() != null && !arg.arg().isEmpty()) {
                return buildSubArg(arg);
            }

            if (arg.ifArg() != null) {
                return buildIfArg(arg.ifArg());
            }


            throw new SquigglyParseException(origin(arg), "Unknown arg type [%s]", arg.getText());
        }

        private ArgumentNode.Builder buildNull(SquigglyGrammarParser.ArgContext arg) {
            return baseArg(arg, ArgumentNode.Type.NULL).value(null);
        }

        private ArgumentNode.Builder buildIfArg(SquigglyGrammarParser.IfArgContext context) {
            Stream<IfNode.IfClause> ifClauseStream = Stream.of(context.ifClause())
                    .map(this::buildIfClause);

            Stream<IfNode.IfClause> elifClauseStream = context.elifClause()
                    .stream()
                    .map(this::buildIfClause);

            List<IfNode.IfClause> ifClauses = Stream.concat(ifClauseStream, elifClauseStream)
                    .collect(toList());

            ArgumentNode elseClause;

            if (context.elseClause() == null) {
                elseClause = baseArg(context, ArgumentNode.Type.NULL).value(null).index(0).build();
            } else {
                elseClause = buildArg(context.elseClause().arg()).index(0).build();
            }

            SquigglyNodeOrigin origin = origin(context);
            FunctionNode functionNode = SquigglyNodes.function(origin)
                    .name(SystemFunctionName.SELF.getFunctionName())
                    .argument(baseArg(context, ArgumentNode.Type.IF).value(SquigglyNodes.ifNode(origin, elseClause, ifClauses)))
                    .build();


            return baseArg(context, ArgumentNode.Type.FUNCTION_CHAIN)
                    .value(Collections.singletonList(functionNode));
        }

        @SuppressWarnings("Duplicates")
        private IfNode.IfClause buildIfClause(SquigglyGrammarParser.IfClauseContext context) {
            ArgumentNode condition = buildArg(context.arg(0)).index(0).build();
            ArgumentNode value = buildArg(context.arg(1)).index(1).build();
            return SquigglyNodes.ifClause(origin(context), condition, value);
        }

        @SuppressWarnings("Duplicates")
        private IfNode.IfClause buildIfClause(SquigglyGrammarParser.ElifClauseContext context) {
            ArgumentNode condition = buildArg(context.arg(0)).index(0).build();
            ArgumentNode value = buildArg(context.arg(1)).index(1).build();
            return SquigglyNodes.ifClause(origin(context), condition, value);
        }

        private ArgumentNode.Builder buildLambda(SquigglyGrammarParser.LambdaContext lambda) {
            List<String> arguments = lambda.lambdaArg()
                    .stream()
                    .map(arg -> arg.variable() == null ? "_" : buildVariableValue(arg.variable()))
                    .collect(toList());

            SquigglyNodeOrigin origin = origin(lambda);
            FunctionNode body = SquigglyNodes.function(origin)
                    .name(SystemFunctionName.SELF.getFunctionName())
                    .argument(buildArg(lambda.lambdaBody().arg()))
                    .build();


            LambdaNode lambdaNode = SquigglyNodes.lambda(origin, body, arguments);

            return baseArg(lambda, ArgumentNode.Type.LAMBDA)
                    .value(lambdaNode);
        }

        private ArgumentNode.Builder buildSubArg(SquigglyGrammarParser.ArgContext arg) {
            if (arg.ParenLeft() != null) {
                ArgumentNode.Builder groupArg = buildArg(arg.arg(0));

                if (arg.argChainLink() != null) {
                    List<FunctionNode> functionNodes = new ArrayList<>(arg.argChainLink().size() + 1);

                    functionNodes.add(SquigglyNodes.function(origin(arg))
                            .name(SystemFunctionName.SELF.getFunctionName())
                            .argument(groupArg)
                            .build()
                    );

                    for (SquigglyGrammarParser.ArgChainLinkContext linkContext : arg.argChainLink()) {
                        functionNodes.add(buildFunction(linkContext, true).build());
                    }


                    groupArg = baseArg(arg, ArgumentNode.Type.FUNCTION_CHAIN).value(functionNodes);
                }

                return groupArg;

            }

            return buildArgExpression(arg);
        }

        private ArgumentNode.Builder buildArgExpression(SquigglyGrammarParser.ArgContext arg) {
            String op = getOp(arg);

            SquigglyNodeOrigin origin = origin(arg);

            FunctionNode.Builder functionNode = SquigglyNodes.function(origin)
                    .name(op);

            arg.arg().forEach(p -> functionNode.argument(buildArg(p)));

            return baseArg(arg, ArgumentNode.Type.FUNCTION_CHAIN)
                    .value(Collections.singletonList(functionNode.build()));
        }

        private String getOp(SquigglyGrammarParser.ArgContext arg) {
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

            if (arg.Elvis() != null) {
                return SystemFunctionName.DEFAULT.getFunctionName();
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

            throw new SquigglyParseException(origin(arg), "unknown op [%s]", arg.getText());
        }

        private boolean matchOp(TerminalNode token1, TerminalNode token2) {
            return token1 != null || token2 != null;
        }

        private boolean matchOp(TerminalNode token1, TerminalNode token2, TerminalNode token3) {
            return token1 != null || token2 != null || token3 != null;
        }

        private ArgumentNode.Builder buildLiteral(SquigglyGrammarParser.LiteralContext context) {
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

            throw new SquigglyParseException(origin(context), "Unknown literal type [%s]", context.getText());
        }

        private ArgumentNode.Builder buildIntRange(SquigglyGrammarParser.IntRangeContext context) {
            boolean exclusiveEnd = context.intRangeOp().Colon() != null;
            ArgumentNode.Builder start;
            ArgumentNode.Builder end;

            if (context.intRangeArg().isEmpty()) {
                start = null;
                end = null;
            } else if (context.intRangeArg().size() > 1) {
                start = buildIntRangeArg(context.intRangeArg(0));
                end = buildIntRangeArg(context.intRangeArg(1));
            } else {
                SquigglyGrammarParser.IntRangeArgContext intRangeArg = context.intRangeArg(0);
                int cmp = comparePositions(intRangeArg.getStart(), context.intRangeOp().getStart());

                if (cmp < 0) {
                    start = buildIntRangeArg(intRangeArg);
                    end = null;
                } else {
                    start = null;
                    end = buildIntRangeArg(intRangeArg);
                }
            }

            if (start == null) {
                start = baseArg(context, ArgumentNode.Type.INTEGER).value(0);
            }

            SquigglyNodeOrigin origin = origin(context);
            IntRangeNode intRangeNode;

            if (end == null || exclusiveEnd) {
                intRangeNode = SquigglyNodes.intRangeExclusive(origin, start, end);
            } else {
                intRangeNode = SquigglyNodes.intRangeInclusive(origin, start, end);
            }


            return baseArg(context, ArgumentNode.Type.INT_RANGE)
                    .value(intRangeNode);
        }

        private FunctionNode buildIntRangeFunction(SquigglyGrammarParser.IntRangeContext intRange) {
            ArgumentNode.Builder arg = buildIntRange(intRange);
            return SquigglyNodes.function(origin(intRange))
                    .name(SystemFunctionName.SLICE.getFunctionName())
                    .argument(baseArg(intRange, ArgumentNode.Type.INPUT).value(ArgumentNode.Type.INPUT))
                    .argument(arg)
                    .build();
        }

        private ArgumentNode.Builder buildIntRangeArg(SquigglyGrammarParser.IntRangeArgContext context) {
            if (context.variable() != null) {
                return buildVariable(context.variable());
            }

            if (context.IntegerLiteral() != null) {
                return buildInteger(context);
            }


            throw new SquigglyParseException(origin(context), "Unknown int range arg type [%s]", context.getText());
        }

        private ArgumentNode.Builder baseArg(ParserRuleContext context, ArgumentNode.Type type) {
            SquigglyNodeOrigin origin = origin(context);
            return SquigglyNodes.argument(origin).type(type);
        }

        private ArgumentNode.Builder buildBoolean(ParserRuleContext context) {
            return baseArg(context, ArgumentNode.Type.BOOLEAN)
                    .value("true".equalsIgnoreCase(context.getText()));
        }

        private ArgumentNode.Builder buildFloat(ParserRuleContext context) {
            return baseArg(context, ArgumentNode.Type.FLOAT).value(Double.parseDouble(context.getText()));
        }

        private ArgumentNode.Builder buildArgChain(SquigglyGrammarParser.ArgChainContext context) {
            int functionLength = (context.argChainLink() == null) ? 0 : context.argChainLink().size();
            functionLength += 2;
            List<FunctionNode> functionNodes = new ArrayList<>(functionLength);

            if (context.arrayDeclaration() != null) {
                functionNodes.add(SquigglyNodes.function(origin(context.arrayDeclaration()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildArrayDeclaration(context.arrayDeclaration()))
                        .build()
                );
            }

            if (context.literal() != null) {
                functionNodes.add(SquigglyNodes.function(origin(context.literal()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildLiteral(context.literal()))
                        .build()
                );
            }

            if (context.intRange() != null) {
                functionNodes.add(SquigglyNodes.function(origin(context.intRange()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildIntRange(context.intRange()))
                        .build()
                );
            }

            if (context.objectDeclaration() != null) {
                functionNodes.add(SquigglyNodes.function(origin(context.objectDeclaration()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildObjectDeclaration(context.objectDeclaration()))
                        .build()
                );
            }


            if (context.variable() != null) {
                functionNodes.add(SquigglyNodes.function(origin(context.variable()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildVariable(context.variable()))
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
                    functionNodes.add(buildFunction(context.initialPropertyAccessor().function(), null, true).build());
                }
            }

            if (context.function() != null) {
                boolean input = !functionNodes.isEmpty();
                functionNodes.add(buildFunction(context.function(), null, input).ascending(ascending).build());
            }

            if (context.argChainLink() != null) {
                boolean input = !functionNodes.isEmpty();

                for (SquigglyGrammarParser.ArgChainLinkContext linkContext : context.argChainLink()) {
                    functionNodes.add(buildFunction(linkContext, input).ascending(ascending).build());
                }
            }

            return baseArg(context, ArgumentNode.Type.FUNCTION_CHAIN)
                    .value(functionNodes);
        }

        private ArgumentNode.Builder buildArrayDeclaration(SquigglyGrammarParser.ArrayDeclarationContext context) {
            if (context.intRange() != null) {
                return baseArg(context, ArgumentNode.Type.ARRAY_RANGE_DECLARATION)
                        .value(buildIntRange(context.intRange()).index(0).build().getValue());
            }


            List<ArgumentNode> argumentNodes = new ArrayList<>(context.arg().size());

            for (int i = 0; i < context.arg().size(); i++) {
                SquigglyGrammarParser.ArgContext arg = context.arg().get(i);
                argumentNodes.add(buildArg(arg).index(i).build());
            }

            return baseArg(context, ArgumentNode.Type.ARRAY_DECLARATION)
                    .value(argumentNodes);
        }

        private ArgumentNode.Builder buildInteger(ParserRuleContext context) {
            return baseArg(context, ArgumentNode.Type.INTEGER).value(Integer.parseInt(context.getText()));
        }

        private ArgumentNode.Builder buildObjectDeclaration(SquigglyGrammarParser.ObjectDeclarationContext context) {
            return baseArg(context, ArgumentNode.Type.OBJECT_DECLARATION)
                    .value(
                            context.objectKeyValue()
                                    .stream()
                                    .map(this::buildObjectArgPair)
                                    .collect(toList())
                    );
        }

        private CorePair<ArgumentNode, ArgumentNode> buildObjectArgPair(SquigglyGrammarParser.ObjectKeyValueContext context) {
            ArgumentNode key = buildObjectArgKey(context.objectKey()).index(0).build();
            ArgumentNode value = buildArg(context.objectValue().arg()).index(0).build();

            return CorePair.of(key, value);
        }

        private ArgumentNode.Builder buildObjectArgKey(SquigglyGrammarParser.ObjectKeyContext context) {
            if (context.Identifier() != null) {
                return baseArg(context, ArgumentNode.Type.STRING).value(context.Identifier().getText());
            } else if (context.literal() != null) {
                return buildLiteral(context.literal());
            } else if (context.variable() != null) {
                return buildVariable(context.variable());
            }

            throw new SquigglyParseException(origin(context), "unknown object arg key [%s]", context.getText());
        }

        private ArgumentNode.Builder buildRegex(ParserRuleContext context) {
            return baseArg(context, ArgumentNode.Type.REGEX).value(buildPattern(context.getText(), context));
        }

        private ArgumentNode.Builder buildString(ParserRuleContext context) {
            return baseArg(context, ArgumentNode.Type.STRING).value(unescapeString(context.getText()));
        }

        private ExpressionNode.Builder expressionBuilder(ParserRuleContext ruleContext, SquigglyName name) {
            return expressionBuilder(ruleContext, name, null, null, null);
        }

        private ExpressionNode.Builder expressionBuilder(ParserRuleContext ruleContext, SquigglyName name, ExpressionNode.Builder parent) {
            return expressionBuilder(ruleContext, name, parent, null, null);
        }

        private ExpressionNode.Builder expressionBuilder(ParserRuleContext ruleContext, SquigglyName name, @Nullable ExpressionNode.Builder parent, SquigglyGrammarParser.NestedExpressionContext nestedExpression, SquigglyGrammarParser.KeyValueFieldArgChainContext keyValueFieldArgChain) {

            ExpressionNode.Builder node = SquigglyNodes.expression(origin(ruleContext)).name(name);

            if (parent != null) {
                node = parent.child(node);
            }

            if (keyValueFieldArgChain != null) {
                node.keyFunctions(parseKeyFunctionChain(keyValueFieldArgChain));
                node.valueFunctions(parseValueFunctionChain(keyValueFieldArgChain));
            }

            if (nestedExpression != null) {
                node.nested(true);

                if (nestedExpression.expressionList() != null) {
                    handleExpressionList(nestedExpression.expressionList(), node);
                }
            }

            return node;
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
            } else if (text.startsWith("\"") && text.endsWith("\"")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            } else if (text.startsWith("`") && text.endsWith("`")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            } else if (text.startsWith("@s(") && text.endsWith(")")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(3, text.length() - 1));
            }

            return text;
        }

        private ArgumentNode.Builder buildVariable(SquigglyGrammarParser.VariableContext context) {
            return baseArg(context, ArgumentNode.Type.VARIABLE).value(buildVariableValue(context));
        }

        private String buildVariableValue(SquigglyGrammarParser.VariableContext context) {
            String text = context.getText();

            if (text.charAt(1) == '{') {
                text = text.substring(2, text.length() - 1);
            } else {
                text = text.substring(1);
            }

            return unescapeString(text);
        }

        private int comparePositions(Token token1, Token token2) {
            int cmp = Integer.compare(token1.getLine(), token2.getLine());

            if (cmp != 0) {
                return cmp;
            }

            return Integer.compare(token1.getCharPositionInLine(), token2.getCharPositionInLine());
        }

        private SquigglyName createName(SquigglyGrammarParser.FieldContext ctx) {
            SquigglyName name;

            if (ctx.StringLiteral() != null) {
                name = SquigglyNames.exact(unescapeString(ctx.StringLiteral().getText()));
            } else if (ctx.exactField() != null) {
                name = SquigglyNames.exact(ctx.exactField().getText());
            } else if (ctx.wildcardField() != null) {
                if ("*".equals(ctx.wildcardField().getText())) {
                    name = SquigglyNames.anyShallow();
                } else {
                    name = SquigglyNames.wildcard(ctx.wildcardField().getText());
                }
            } else if (ctx.RegexLiteral() != null) {
                Pattern pattern = buildPattern(ctx.RegexLiteral().getText(), ctx);
                name = SquigglyNames.regex(pattern.pattern(), pattern);
            } else {
                throw new SquigglyParseException(origin(ctx), "unhandled field [%s]", ctx.getText());
            }

            return name;
        }

        private Pattern buildPattern(String fullPattern, ParserRuleContext ctx) {
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
                        case "m":
                            flagMask |= Pattern.MULTILINE;
                            break;
                        case "s":
                            flagMask |= Pattern.UNIX_LINES;
                            break;
                        case "x":
                            flagMask |= Pattern.COMMENTS;
                            break;
                        default:
                            throw new SquigglyParseException(origin(ctx), "Unrecognized flag %s for patterh %s", flag, pattern);
                    }
                }
            }

            return Pattern.compile(pattern, flagMask);
        }


        private void handleNegatedExpression(SquigglyGrammarParser.NegatedExpressionContext ctx, ExpressionNode.Builder parent) {
            if (ctx.field() != null) {
                expressionBuilder(ctx.field(), createName(ctx.field()), parent).negated(true);
            } else if (ctx.dottedField() != null) {
                for (int i = 0; i < ctx.dottedField().field().size(); i++) {
                    SquigglyGrammarParser.FieldContext fieldContext = ctx.dottedField().field(i);
                    parent.nested(true);

                    ExpressionNode.Builder builder = SquigglyNodes.expression(origin(ctx.dottedField()))
                            .name(createName(fieldContext))
                            .negativeParent(true);

                    parent = parent.child(builder.dotPathed(true));
                }

                parent.negated(true);
                parent.negativeParent(false);
            }
        }

    }

    private FilterNode.Builder analyze(FilterNode.Builder filterNode) {
        for (StatementNode.Builder statementNode : filterNode.getStatements()) {
            analyze(statementNode);
        }

        return filterNode;
    }

    private void analyze(StatementNode.Builder statementNode) {
        analyze(statementNode.getRoot());
    }

    private void analyze(ExpressionNode.Builder expressionNode) {
        Map<ExpressionNode.Builder, ExpressionNode.Builder> nodesToAdd = new IdentityHashMap<>();
        analyze(expressionNode, nodesToAdd);

        for (Map.Entry<ExpressionNode.Builder, ExpressionNode.Builder> entry : nodesToAdd.entrySet()) {
            entry.getKey().child(entry.getValue());
        }
    }

    private void analyze(ExpressionNode.Builder node, Map<ExpressionNode.Builder, ExpressionNode.Builder> nodesToAdd) {
        Map<String, ExpressionNode.Builder> children = node.getChildren();

        if (children != null && !children.isEmpty()) {
            boolean allNegated = true;

            for (ExpressionNode.Builder child : children.values()) {
                if (!ExpressionNode.Modifier.isNegated(child.getModifiers()) && !child.isNegativeParent()) {
                    allNegated = false;
                    break;
                }
            }

            if (allNegated) {
                nodesToAdd.put(node, SquigglyNodes.expression(node.getOrigin()).
                        name(newBaseViewName())
                        .dotPathed(node.isDotPathed()));
            }

            for (ExpressionNode.Builder child : node.getChildren().values()) {
                analyze(child, nodesToAdd);
            }
        }
    }


    private SquigglyName newBaseViewName() {
        return SquigglyNames.exact(PropertyView.BASE_VIEW);
    }
}

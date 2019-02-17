package com.github.bohnman.squiggly.core.parser;

import com.github.bohnman.core.antlr4.ThrowingErrorListener;
import com.github.bohnman.core.cache.CoreCache;
import com.github.bohnman.core.cache.CoreCacheBuilder;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.core.tuple.CorePair;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.config.SystemFunctionName;
import com.github.bohnman.squiggly.core.metric.SquigglyMetrics;
import com.github.bohnman.squiggly.core.metric.source.CoreCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.core.name.*;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionBaseVisitor;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionLexer;
import com.github.bohnman.squiggly.core.parser.antlr4.SquigglyExpressionParser;
import com.github.bohnman.squiggly.core.parser.node.*;
import com.github.bohnman.squiggly.core.view.PropertyView;
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

/**
 * The parser takes a filter expression and compiles it to an Abstract Syntax Tree (AST).  In this parser's case, the
 * tree doesn't have a root node but rather just returns top level nodes.
 */
@ThreadSafe
public class SquigglyParser {


    public static final String SELF_REFERENCE = "$";
    public static final String PARENT_REFERENCE = "$$";

    // Caches parsed filter expressions
    private final CoreCache<String, List<SquigglyNode>> cache;

    public SquigglyParser(SquigglyConfig config, SquigglyMetrics metrics) {
        cache = CoreCacheBuilder.from(config.getParserNodeCacheSpec()).build();
        metrics.add(new CoreCacheSquigglyMetricsSource("squiggly.parser.nodeCache.", cache));
    }

    /**
     * Parse node filter expression.
     *
     * @param filter filter
     * @return list of squiggly nodes
     */
    public List<SquigglyNode> parseNodeFilter(String filter) {
        filter = CoreStrings.trim(filter);

        if (CoreStrings.isEmpty(filter)) {
            return Collections.emptyList();
        }

        List<SquigglyNode> cachedNodes = cache.get(filter);

        if (cachedNodes != null) {
            return cachedNodes;
        }

        SquigglyExpressionLexer lexer = ThrowingErrorListener.overwrite(new SquigglyExpressionLexer(CharStreams.fromString(filter)));
        SquigglyExpressionParser parser = ThrowingErrorListener.overwrite(new SquigglyExpressionParser(new CommonTokenStream(lexer)));
        NodeFilterVisitor visitor = new NodeFilterVisitor();
        List<SquigglyNode> nodes = visitor.visit(parser.nodeFilter());

        cache.put(filter, nodes);
        return nodes;
    }

    /**
     * Parse a filter expression.
     *
     * @param filter the filter expression
     * @return compiled nodes
     */
    public SquigglyNode parsePropertyFilter(String filter) {
        filter = CoreStrings.trim(filter);

        if (CoreStrings.isEmpty(filter)) {
            return SquigglyNode.EMPTY;
        }

        // get it from the cache if we can
        List<SquigglyNode> cachedNodes = cache.get(filter);

        if (cachedNodes != null) {
            return cachedNodes.isEmpty() ? SquigglyNode.EMPTY : cachedNodes.get(0);
        }

        SquigglyExpressionLexer lexer = ThrowingErrorListener.overwrite(new SquigglyExpressionLexer(CharStreams.fromString(filter)));
        SquigglyExpressionParser parser = ThrowingErrorListener.overwrite(new SquigglyExpressionParser(new CommonTokenStream(lexer)));

        PropertyFilterVisitor visitor = new PropertyFilterVisitor();
        SquigglyNode node = visitor.visit(parser.propertyFilter());

        if (node != null) {
            cache.put(filter, Collections.singletonList(node));
        }

        return node;
    }

    private class NodeFilterVisitor extends SquigglyExpressionBaseVisitor<List<SquigglyNode>> {

        private final PropertyFilterVisitor visitor = new PropertyFilterVisitor();

        @Override
        public List<SquigglyNode> visitNodeFilter(SquigglyExpressionParser.NodeFilterContext ctx) {
            return ctx.nodeExpressionList()
                    .stream()
                    .map(visitor::visitNodeExpressionList)
                    .filter(Objects::nonNull)
                    .collect(toList());
        }
    }

    private class PropertyFilterVisitor extends SquigglyExpressionBaseVisitor<SquigglyNode> {

        @Override
        public SquigglyNode visitNodeExpressionList(SquigglyExpressionParser.NodeExpressionListContext ctx) {
            MutableNode root = createMutableNode(ctx, null, new ExactName(SquigglyNode.ROOT)).dotPathed(true);

            if (ctx.expressionList() != null) {
                handleExpressionList(ctx.expressionList(), root);
            } else if (ctx.topLevelExpression() != null) {
                handleTopLevelExpression(ctx.topLevelExpression(), root);
            }

            MutableNode analyzedRoot = analyze(root);
            return analyzedRoot.toSquigglyNode();
        }

        @Override
        public SquigglyNode visitPropertyFilter(SquigglyExpressionParser.PropertyFilterContext ctx) {
            MutableNode root = createMutableNode(ctx, null, new ExactName(SquigglyNode.ROOT)).dotPathed(true);
            handleExpressionList(ctx.expressionList(), root);
            MutableNode analyzedRoot = analyze(root);
            return analyzedRoot.toSquigglyNode();
        }

        private ParseContext parseContext(ParserRuleContext ctx) {
            Token start = ctx.getStart();
            return new ParseContext(start.getLine(), start.getCharPositionInLine());
        }

        private void handleTopLevelExpression(SquigglyExpressionParser.TopLevelExpressionContext context, MutableNode root) {
            root.name = new AnyDeepName();
            if (context.assignment() != null) {
                root.valueFunctions(parseAssignment(context.assignment()));
                return;
            }

            if (!context.argChainLink().isEmpty()) {
                root.valueFunctions(context.argChainLink()
                        .stream()
                        .map(argChainLink -> buildFunction(argChainLink, true).build())
                        .collect(Collectors.toList())
                );
            }
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

            throw new SquigglyParseException(parseContext(ctx), "Unhandled expression [%s].", ctx.getText());

        }


        private void handleDottedFieldExpression(SquigglyExpressionParser.DottedFieldExpressionContext ctx, MutableNode parent) {
            SquigglyExpressionParser.DottedFieldContext dottedField = ctx.dottedField();

            parent.nested(dottedField.field().size() > 1);

            for (int i = 0; i < dottedField.field().size() - 1; i++) {
                SquigglyExpressionParser.FieldContext field = dottedField.field(i);
                parent = createMutableNode(field, parent, createName(field)).dotPathed(true);
                parent.nested(true);
            }

            SquigglyExpressionParser.FieldContext lastField = ctx.dottedField().field(dottedField.field().size() - 1);
            createMutableNode(lastField, parent, createName(lastField), ctx.nestedExpression(), ctx.keyValueFieldArgChain());
        }

        private void handleFieldGroupExpression(SquigglyExpressionParser.FieldGroupExpressionContext ctx, MutableNode parent) {
            SquigglyExpressionParser.FieldGroupContext fieldGroup = ctx.fieldGroup();

            for (SquigglyExpressionParser.FieldContext fieldContext : fieldGroup.field()) {
                createMutableNode(fieldContext, parent, createName(fieldContext), ctx.nestedExpression(), ctx.keyValueFieldArgChain());
            }
        }

        private void handleDeepExpression(SquigglyExpressionParser.DeepExpressionContext ctx, MutableNode parent) {
            List<SquigglyExpressionParser.DeepArgContext> deepArgs = ctx.deepArg();

            Integer minDepth = null;
            Integer maxDepth = null;
            boolean maxDepthExclusive = false;
            SquigglyExpressionParser.DeepRangeContext deepRange = ctx.deepRange();

            if (deepRange != null && !deepRange.IntegerLiteral().isEmpty()) {
                SquigglyExpressionParser.IntRangeOpContext intRangeOp = deepRange.intRangeOp();
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
                throw new SquigglyParseException(parseContext(ctx), "Invalid deep range %s", ctx.getText());
            }

            if (deepArgs.isEmpty()) {
                MutableNode node = createMutableNode(ctx, parent, AnyDeepName.get())
                        .deep(true);

                if (minDepth != null) {
                    node.minDepth(node.depth + minDepth);
                }

                if (maxDepth != null) {
                    node.maxDepth(node.depth + maxDepth);
                }
                return;
            }

            for (SquigglyExpressionParser.DeepArgContext deepArg : deepArgs) {
                for (MutableNode node : handleDeepArg(deepArg, parent)) {
                    node.deep(true).minDepth(minDepth).maxDepth(maxDepth);
                }
            }
        }

        private List<MutableNode> handleDeepArg(SquigglyExpressionParser.DeepArgContext ctx, MutableNode parent) {
            if (ctx.Subtract() != null) {
                return Collections.singletonList(parent.addChild(new MutableNode(parseContext(ctx.field()), createName(ctx.field())).negated(true)));
            }

            List<SquigglyExpressionParser.FieldContext> fields;
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
                    .map(field -> createMutableNode(ruleContext, parent, createName(field), null, ctx.keyValueFieldArgChain()))
                    .collect(toList());
        }


        private MutableNode createMutableNode(ParserRuleContext ruleContext, MutableNode parent, SquigglyName name) {
            return createMutableNode(ruleContext, parent, name, null, null);
        }

        private MutableNode createMutableNode(ParserRuleContext ruleContext, @Nullable  MutableNode parent, SquigglyName name, SquigglyExpressionParser.NestedExpressionContext nestedExpression, SquigglyExpressionParser.KeyValueFieldArgChainContext keyValueFieldArgChain) {

            MutableNode node = new MutableNode(parseContext(ruleContext), name);

            if (parent != null) {
                node = parent.addChild(node);
                node.depth(parent.depth + 1);
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

        private List<FunctionNode> parseKeyFunctionChain(SquigglyExpressionParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty()) {
                return Collections.emptyList();
            }

            return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(0));
        }

        private List<FunctionNode> parseValueFunctionChain(SquigglyExpressionParser.KeyValueFieldArgChainContext context) {
            if (context.Colon().isEmpty()) {
                return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(0));
            } else if (context.keyValueFieldArgChainLink().size() > 1) {
                return parseKeyValueFieldArgChainLink(context.keyValueFieldArgChainLink().get(1));
            }

            return Collections.emptyList();
        }

        private List<FunctionNode> parseKeyValueFieldArgChainLink(SquigglyExpressionParser.KeyValueFieldArgChainLinkContext context) {
            if (context.assignment() != null) {
                return parseAssignment(context.assignment());
            }

            if (context.function() != null) {
                List<FunctionNode> functionNodes = new ArrayList<>(context.argChainLink().size() + 1);
                functionNodes.add(buildFunction(context.function(), null, true).build());

                for (SquigglyExpressionParser.ArgChainLinkContext linkContext : context.argChainLink()) {
                    functionNodes.add(buildFunction(linkContext, true).build());
                }

                return functionNodes;
            }

            throw new SquigglyParseException(parseContext(context), "Unhandled keyValueArgChainLink [%s]", context.getText());
        }

        private List<FunctionNode> parseAssignment(SquigglyExpressionParser.AssignmentContext context) {

            FunctionNodeType type = FunctionNodeType.ASSIGNMENT;

            String name = SystemFunctionName.ASSIGN.getFunctionName();

            return Collections.singletonList(FunctionNode.builder()
                    .context(parseContext(context))
                    .name(name)
                    .type(type)
                    .argument(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                    .argument(buildArg(context.arg(), context))
                    .build());

        }

        private FunctionNode.Builder buildFunction(SquigglyExpressionParser.ArgChainLinkContext context, boolean input) {
            if (context.functionAccessor() != null) {
                return buildFunction(context.functionAccessor().function(), context.functionAccessor().accessOperator(), input);
            }

            if (context.propertyAccessor() != null) {
                return buildPropertyFunction(context.propertyAccessor(), input);
            }

            throw new SquigglyParseException(parseContext(context), "unknown arg chain link [%s]", context.getText());
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
            } else if (context.intRange() != null) {
                value = Collections.singletonList(buildIntRangeFunction(context.intRange()));
                type = ArgumentNodeType.FUNCTION_CHAIN;
            } else {
                throw new SquigglyParseException(parseContext(context), "Cannot find property name [%s]", context.getText());
            }

            return buildBasePropertyFunction(context)
                    .argument(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildPropertyFunction(SquigglyExpressionParser.InitialPropertyAccessorContext context) {
            Object value;
            ArgumentNodeType type;

            if (context.Identifier() != null) {
                value = context.Identifier().getText();
                type = ArgumentNodeType.STRING;
            } else if (context.DollarDollar() != null) {
                value = context.DollarDollar().getText();
                type = ArgumentNodeType.STRING;
            } else if (context.Dollar() != null) {
                value = context.Dollar().getText();
                type = ArgumentNodeType.STRING;
            } else {
                throw new SquigglyParseException(parseContext(context), "Unknown initial property accessor [%s]", context.getText());
            }

            return buildBasePropertyFunction(context)
                    .initial(true)
                    .argument(baseArg(context, type).value(value));
        }

        private FunctionNode.Builder buildBasePropertyFunction(ParserRuleContext context) {
            FunctionNode.Builder function = FunctionNode.builder()
                    .context(parseContext(context))
                    .name(SystemFunctionName.PROPERTY.getFunctionName())
                    .type(FunctionNodeType.PROPERTY)
                    .argument(baseArg(context, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                    .ignoreNulls(true);

            return function;
        }

        private FunctionNode.Builder buildFunction(SquigglyExpressionParser.FunctionContext functionContext, @Nullable SquigglyExpressionParser.AccessOperatorContext operatorContext, boolean input) {
            ParseContext context = parseContext(functionContext);
            FunctionNode.Builder builder = buildBaseFunction(functionContext, context);

            if (input) {
                builder.argument(baseArg(functionContext, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT));
            }

            applyParameters(builder, functionContext);

            return builder;
        }

        private FunctionNode.Builder buildBaseFunction(SquigglyExpressionParser.FunctionContext functionContext, ParseContext context) {
            String text = functionContext.functionName().getText();
            String name;

            if (text.length() == 1) {
                name = SystemFunctionName.PICK.getFunctionName();
            } else {
                name = text.substring(1).trim();
            }

            return FunctionNode.builder()
                    .context(context)
                    .name(name);
        }

        private void applyParameters(FunctionNode.Builder builder, SquigglyExpressionParser.FunctionContext functionContext) {
            functionContext.arg().forEach(parameter -> applyParameter(builder, parameter));
        }

        private void applyParameter(FunctionNode.Builder builder, SquigglyExpressionParser.ArgContext parameter) {
            ArgumentNode.Builder arg = buildArg(parameter);
            builder.argument(arg);
        }

        private ArgumentNode.Builder buildArg(SquigglyExpressionParser.ArgContext arg) {
            return buildArg(arg, arg);
        }

        private ArgumentNode.Builder buildArg(SquigglyExpressionParser.ArgContext arg, ParserRuleContext context) {
            Object value;
            ArgumentNodeType type;

            if (arg == null) {
                return baseArg(context, ArgumentNodeType.NULL).value(null);
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


            throw new SquigglyParseException(parseContext(arg), "Unknown arg type [%s]", arg.getText());
        }

        private ArgumentNode.Builder buildNull(SquigglyExpressionParser.ArgContext arg) {
            return baseArg(arg, ArgumentNodeType.NULL).value(null);
        }

        private ArgumentNode.Builder buildIfArg(SquigglyExpressionParser.IfArgContext context) {
            Stream<IfNode.IfClause> ifClauseStream = Stream.of(context.ifClause())
                    .map(this::buildIfClause);

            Stream<IfNode.IfClause> elifClauseStream = context.elifClause()
                    .stream()
                    .map(this::buildIfClause);

            List<IfNode.IfClause> ifClauses = Stream.concat(ifClauseStream, elifClauseStream)
                    .collect(toList());

            ArgumentNode elseClause;

            if (context.elseClause() == null) {
                elseClause = baseArg(context, ArgumentNodeType.NULL).value(null).index(0).build();
            } else {
                elseClause = buildArg(context.elseClause().arg()).index(0).build();
            }

            FunctionNode functionNode = FunctionNode.builder()
                    .context(parseContext(context))
                    .name(SystemFunctionName.SELF.getFunctionName())
                    .argument(baseArg(context, ArgumentNodeType.IF).value(new IfNode(ifClauses, elseClause)))
                    .build();


            return baseArg(context, ArgumentNodeType.FUNCTION_CHAIN)
                    .value(Collections.singletonList(functionNode));
        }

        @SuppressWarnings("Duplicates")
        private IfNode.IfClause buildIfClause(SquigglyExpressionParser.IfClauseContext context) {
            ArgumentNode condition = buildArg(context.arg(0)).index(0).build();
            ArgumentNode value = buildArg(context.arg(1)).index(1).build();
            return new IfNode.IfClause(condition, value);
        }

        @SuppressWarnings("Duplicates")
        private IfNode.IfClause buildIfClause(SquigglyExpressionParser.ElifClauseContext context) {
            ArgumentNode condition = buildArg(context.arg(0)).index(0).build();
            ArgumentNode value = buildArg(context.arg(1)).index(1).build();
            return new IfNode.IfClause(condition, value);
        }

        private ArgumentNode.Builder buildLambda(SquigglyExpressionParser.LambdaContext lambda) {
            List<String> arguments = lambda.lambdaArg()
                    .stream()
                    .map(arg -> arg.variable() == null ? "_" : buildVariableValue(arg.variable()))
                    .collect(toList());

            ParseContext parseContext = parseContext(lambda);
            FunctionNode body = FunctionNode.builder()
                    .name(SystemFunctionName.SELF.getFunctionName())
                    .context(parseContext)
                    .argument(buildArg(lambda.lambdaBody().arg()))
                    .build();


            LambdaNode lambdaNode = new LambdaNode(parseContext, arguments, body);

            return baseArg(lambda, ArgumentNodeType.LAMBDA)
                    .value(lambdaNode);
        }

        private ArgumentNode.Builder buildSubArg(SquigglyExpressionParser.ArgContext arg) {
            if (arg.ParenLeft() != null) {
                ArgumentNode.Builder groupArg = buildArg(arg.arg(0));

                if (arg.argChainLink() != null) {
                    List<FunctionNode> functionNodes = new ArrayList<>(arg.argChainLink().size() + 1);

                    functionNodes.add(FunctionNode.builder()
                            .context(parseContext(arg))
                            .name(SystemFunctionName.SELF.getFunctionName())
                            .argument(groupArg)
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

            arg.arg().forEach(p -> functionNode.argument(buildArg(p)));

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

            throw new SquigglyParseException(parseContext(arg), "unknown op [%s]", arg.getText());
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

            throw new SquigglyParseException(parseContext(context), "Unknown literal type [%s]", context.getText());
        }

        private ArgumentNode.Builder buildIntRange(SquigglyExpressionParser.IntRangeContext context) {
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
                SquigglyExpressionParser.IntRangeArgContext intRangeArg = context.intRangeArg(0);
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
                start = baseArg(context, ArgumentNodeType.INTEGER).value(0);
            }

            return baseArg(context, ArgumentNodeType.INT_RANGE)
                    .value(new IntRangeNode(start, end, end == null || exclusiveEnd));
        }

        private FunctionNode buildIntRangeFunction(SquigglyExpressionParser.IntRangeContext intRange) {
            ArgumentNode.Builder arg = buildIntRange(intRange);
            return FunctionNode.builder()
                    .context(parseContext(intRange))
                    .name(SystemFunctionName.SLICE.getFunctionName())
                    .argument(baseArg(intRange, ArgumentNodeType.INPUT).value(ArgumentNodeType.INPUT))
                    .argument(arg)
                    .build();
        }

        private ArgumentNode.Builder buildIntRangeArg(SquigglyExpressionParser.IntRangeArgContext context) {
            if (context.variable() != null) {
                return buildVariable(context.variable());
            }

            if (context.IntegerLiteral() != null) {
                return buildInteger(context);
            }


            throw new SquigglyParseException(parseContext(context), "Unknown int range arg type [%s]", context.getText());
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
            return baseArg(context, ArgumentNodeType.FLOAT).value(Double.parseDouble(context.getText()));
        }

        private ArgumentNode.Builder buildArgChain(SquigglyExpressionParser.ArgChainContext context) {
            int functionLength = (context.argChainLink() == null) ? 0 : context.argChainLink().size();
            functionLength += 2;
            List<FunctionNode> functionNodes = new ArrayList<>(functionLength);

            if (context.arrayDeclaration() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.arrayDeclaration()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildArrayDeclaration(context.arrayDeclaration()))
                        .build()
                );
            }

            if (context.literal() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.literal()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildLiteral(context.literal()))
                        .build()
                );
            }

            if (context.intRange() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.intRange()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildIntRange(context.intRange()))
                        .build()
                );
            }

            if (context.objectDeclaration() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.objectDeclaration()))
                        .name(SystemFunctionName.SELF.getFunctionName())
                        .argument(buildObjectDeclaration(context.objectDeclaration()))
                        .build()
                );
            }


            if (context.variable() != null) {
                functionNodes.add(FunctionNode.builder()
                        .context(parseContext(context.variable()))
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
                    functionNodes.add(buildFunction(context.initialPropertyAccessor().function(), null , true).build());
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

        private ArgumentNode.Builder buildArrayDeclaration(SquigglyExpressionParser.ArrayDeclarationContext context) {
            if (context.intRange() != null) {
                return baseArg(context, ArgumentNodeType.ARRAY_RANGE_DECLARATION)
                        .value(buildIntRange(context.intRange()).index(0).build().getValue());
            }


            List<ArgumentNode> argumentNodes = new ArrayList<>(context.arg().size());

            for (int i = 0; i < context.arg().size(); i++) {
                SquigglyExpressionParser.ArgContext arg = context.arg().get(i);
                argumentNodes.add(buildArg(arg).index(i).build());
            }

            return baseArg(context, ArgumentNodeType.ARRAY_DECLARATION)
                    .value(argumentNodes);
        }

        private ArgumentNode.Builder buildInteger(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.INTEGER).value(Integer.parseInt(context.getText()));
        }

        private ArgumentNode.Builder buildObjectDeclaration(SquigglyExpressionParser.ObjectDeclarationContext context) {
            return baseArg(context, ArgumentNodeType.OBJECT_DECLARATION)
                    .value(
                            context.objectKeyValue()
                                    .stream()
                                    .map(this::buildObjectArgPair)
                                    .collect(toList())
                    );
        }

        private CorePair<ArgumentNode, ArgumentNode> buildObjectArgPair(SquigglyExpressionParser.ObjectKeyValueContext context) {
            ArgumentNode key = buildObjectArgKey(context.objectKey()).index(0).build();
            ArgumentNode value = buildArg(context.objectValue().arg()).index(0).build();

            return CorePair.of(key, value);
        }

        private ArgumentNode.Builder buildObjectArgKey(SquigglyExpressionParser.ObjectKeyContext context) {
            if (context.Identifier() != null) {
                return baseArg(context, ArgumentNodeType.STRING).value(context.Identifier().getText());
            } else if (context.literal() != null) {
                return buildLiteral(context.literal());
            } else if (context.variable() != null) {
                return buildVariable(context.variable());
            }

            throw new SquigglyParseException(parseContext(context), "unknown object arg key [%s]", context.getText());
        }

        private ArgumentNode.Builder buildRegex(ParserRuleContext context) {
            return baseArg(context, ArgumentNodeType.REGEX).value(buildPattern(context.getText(), context));
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
            } else if (text.startsWith("\"") && text.endsWith("\"")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            } else if (text.startsWith("`") && text.endsWith("`")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(1, text.length() - 1));
            } else if (text.startsWith("@s(") && text.endsWith(")")) {
                text = CoreStrings.unescapeEcmaScript(text.substring(3, text.length() - 1));
            }

            return text;
        }

        private ArgumentNode.Builder buildVariable(SquigglyExpressionParser.VariableContext context) {
            return baseArg(context, ArgumentNodeType.VARIABLE).value(buildVariableValue(context));
        }

        private String buildVariableValue(SquigglyExpressionParser.VariableContext context) {
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

        private SquigglyName createName(SquigglyExpressionParser.FieldContext ctx) {
            SquigglyName name;

            if (ctx.StringLiteral() != null) {
                name = new ExactName(unescapeString(ctx.StringLiteral().getText()));
            } else if (ctx.exactField() != null) {
                name = new ExactName(ctx.exactField().getText());
            } else if (ctx.wildcardField() != null) {
                if ("*".equals(ctx.wildcardField().getText())) {
                    name = AnyShallowName.get();
                } else {
                    name = new WildcardName(ctx.wildcardField().getText());
                }
            } else if (ctx.RegexLiteral() != null) {
                Pattern pattern = buildPattern(ctx.RegexLiteral().getText(), ctx);
                name = new RegexName(pattern.pattern(), pattern);
            } else if (ctx.variable() != null) {
                name = new VariableName(buildVariableValue(ctx.variable()));
            } else {
                throw new SquigglyParseException(parseContext(ctx), "unhandled field [%s]", ctx.getText());
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
                            throw new SquigglyParseException(parseContext(ctx), "Unrecognized flag %s for patterh %s", flag, pattern);
                    }
                }
            }

            return Pattern.compile(pattern, flagMask);
        }


        private void handleNegatedExpression(SquigglyExpressionParser.NegatedExpressionContext ctx, MutableNode parent) {
            if (ctx.field() != null) {
                createMutableNode(ctx.field(), parent, createName(ctx.field())).negated(true);
            } else if (ctx.dottedField() != null) {
                for (int i = 0; i < ctx.dottedField().field().size(); i++) {
                    SquigglyExpressionParser.FieldContext fieldContext = ctx.dottedField().field(i);
                    parent.nested(true);

                    MutableNode mutableNode = new MutableNode(parseContext(ctx.dottedField()), createName(fieldContext));
                    mutableNode.negativeParent = true;

                    parent = parent.addChild(mutableNode.dotPathed(true));
                }

                parent.negated(true);
                parent.negativeParent = false;
            }
        }

    }

    private MutableNode analyze(MutableNode node) {
        Map<MutableNode, MutableNode> nodesToAdd = new IdentityHashMap<>();
        MutableNode analyze = analyze(node, nodesToAdd);

        for (Map.Entry<MutableNode, MutableNode> entry : nodesToAdd.entrySet()) {
            entry.getKey().addChild(entry.getValue());
        }

        return analyze;
    }

    private MutableNode analyze(MutableNode node, Map<MutableNode, MutableNode> nodesToAdd) {
        if (node.children != null && !node.children.isEmpty()) {
            boolean allNegated = true;

            for (MutableNode child : node.children.values()) {
                if (!SquigglyNode.Modifier.isNegated(child.modifiers) && !child.negativeParent) {
                    allNegated = false;
                    break;
                }
            }

            if (allNegated) {
                nodesToAdd.put(node, new MutableNode(node.getContext(), newBaseViewName()).dotPathed(node.dotPathed));
            }

            for (MutableNode child : node.children.values()) {
                analyze(child, nodesToAdd);
            }
        }

        return node;
    }

    private class MutableNode {
        private boolean negativeParent;
        private final ParseContext context;
        private SquigglyName name;
        private int modifiers;
        private int stage;
        private int depth;
        @Nullable
        private Map<String, MutableNode> children;
        private boolean dotPathed;
        @Nullable
        private MutableNode parent;
        private List<FunctionNode> keyFunctions = new ArrayList<>();
        private List<FunctionNode> valueFunctions = new ArrayList<>();

        @Nullable
        private Integer minDepth;

        @Nullable
        private Integer maxDepth;

        MutableNode(ParseContext context, SquigglyName name) {
            this.context = context;
            this.name = name;
        }

        SquigglyNode toSquigglyNode() {
            if (name == null) {
                throw new SquigglyParseException(context, "no names specified.");
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

            return new SquigglyNode(context, name, modifiers, childNodes, stage, depth, keyFunctions, valueFunctions, minDepth, maxDepth);
        }

        public ParseContext getContext() {
            return context;
        }

        public MutableNode stage(int stage) {
            this.stage = stage;
            return this;
        }

        public MutableNode depth(int depth) {
            this.depth = depth;
            return this;
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

        public MutableNode nested(boolean nested) {
            modifiers = SquigglyNode.Modifier.setNested(modifiers, nested);
            return this;
        }

        public MutableNode negated(boolean negated) {
            modifiers = SquigglyNode.Modifier.setNegated(modifiers, negated);
            return this;
        }

        public MutableNode deep(boolean deep) {
            modifiers = SquigglyNode.Modifier.setDeep(modifiers, deep);
            return this;
        }

        public MutableNode minDepth(Integer minDepth) {
            this.minDepth = minDepth;
            return this;
        }

        public MutableNode maxDepth(Integer maxDepth) {
            this.maxDepth = maxDepth;
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


                existingChild.nested(SquigglyNode.Modifier.isNested(existingChild.modifiers) || SquigglyNode.Modifier.isNested(childToAdd.modifiers));
                existingChild.dotPathed = existingChild.dotPathed && childToAdd.dotPathed;
                existingChild.negativeParent = existingChild.negativeParent && childToAdd.negativeParent;
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

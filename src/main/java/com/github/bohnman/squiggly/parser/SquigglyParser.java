package com.github.bohnman.squiggly.parser;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.metric.source.GuavaCacheSquigglyMetricsSource;
import com.github.bohnman.squiggly.metric.source.SquigglyMetricsSource;
import com.github.bohnman.squiggly.name.AnyDeepName;
import com.github.bohnman.squiggly.name.AnyShallowName;
import com.github.bohnman.squiggly.name.ExactName;
import com.github.bohnman.squiggly.name.RegexName;
import com.github.bohnman.squiggly.name.SquigglyName;
import com.github.bohnman.squiggly.name.WildcardName;
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionBaseListener;
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionLexer;
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionParser;
import com.github.bohnman.squiggly.util.antlr4.ThrowingErrorListener;
import com.github.bohnman.squiggly.view.PropertyView;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import net.jcip.annotations.ThreadSafe;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

/**
 * The parser takes a filter expression and compiles it to an Abstract Syntax Tree (AST).  In this parser's case, the
 * tree doesn't have a root node but rather just returns top level nodes.
 */
@ThreadSafe
public class SquigglyParser {

    // Caches parsed filter expressions
    private static final Cache<String, List<SquigglyNode>> CACHE;
    private static final SquigglyMetricsSource METRICS_SOURCE;

    static {
        CACHE = CacheBuilder.from(SquigglyConfig.getParserNodeCacheSpec()).build();
        METRICS_SOURCE = new GuavaCacheSquigglyMetricsSource("squiggly.parser.nodeCache.", CACHE);
    }

    /**
     * Parse a filter expression.
     *
     * @param filter the filter expression
     * @return compiled nodes
     */
    public List<SquigglyNode> parse(String filter) {
        filter = StringUtils.trim(filter);

        if (StringUtils.isEmpty(filter)) {
            return Collections.emptyList();
        }

        // get it from the cache if we can
        List<SquigglyNode> cachedNodes = CACHE.getIfPresent(filter);

        if (cachedNodes != null) {
            return cachedNodes;
        }


        SquigglyExpressionLexer lexer = ThrowingErrorListener.overwrite(new SquigglyExpressionLexer(new ANTLRInputStream(filter)));
        SquigglyExpressionParser parser = ThrowingErrorListener.overwrite(new SquigglyExpressionParser(new CommonTokenStream(lexer)));

        ParseTreeWalker walker = new ParseTreeWalker();
        Listener listener = new Listener();
        walker.walk(listener, parser.parse());

        List<SquigglyNode> nodes = Collections.unmodifiableList(listener.getNodes());

        CACHE.put(filter, nodes);
        return nodes;
    }

    // Break the filter expression into tokens
    private List<String> tokenize(String filter, String separators) {
        StringTokenizer tokenizer = new StringTokenizer(filter, separators, true);
        List<String> tokens = new ArrayList<>();

        while (tokenizer.hasMoreTokens()) {
            tokens.add(tokenizer.nextToken());
        }

        return tokens;
    }

    public static SquigglyMetricsSource getMetricsSource() {
        return METRICS_SOURCE;
    }

    private class Listener extends SquigglyExpressionBaseListener {

        MutableNode root = new MutableNode();
        MutableNode parent = root;
        MutableNode current;
        boolean emptyNested = false;
        boolean negated = false;
        String regexPattern;
        Set<String> regexFlags;
        int dotPath = 0;
        MutableNode dotCurrent;
        SquigglyName name;

        @Override
        public void enterExpression(SquigglyExpressionParser.ExpressionContext ctx) {
            current = parent.newChild();
        }

        @Override
        public void enterEmpty_nested_expression(SquigglyExpressionParser.Empty_nested_expressionContext ctx) {
            current.emptyNested = true;
        }

        @Override
        public void enterParse(SquigglyExpressionParser.ParseContext ctx) {
            super.enterParse(ctx);
        }


        @Override
        public void enterDot_path(SquigglyExpressionParser.Dot_pathContext ctx) {
            dotPath = 1;
            dotCurrent = current;
        }

        @Override
        public void exitDot_path(SquigglyExpressionParser.Dot_pathContext ctx) {
            dotPath = 0;
            current.negated = negated;
            negated = false;

            if (dotCurrent.emptyNested && dotCurrent != current) {
                current.emptyNested = true;
                dotCurrent.emptyNested = false;
            }

            if (dotCurrent.nested) {
                current.returnParent = dotCurrent;
                parent = current;
            } else {
                current = dotCurrent;
            }

            dotCurrent = null;
        }

        @Override
        public void enterNested_expression(SquigglyExpressionParser.Nested_expressionContext ctx) {
            current.squiggly = true;
            current.nested = true;
            parent = current;
        }

        @Override
        public void exitNested_expression(SquigglyExpressionParser.Nested_expressionContext ctx) {
            if (parent.returnParent == null) {
                parent = parent.parent;
            }  else {
                parent = parent.returnParent.parent;
            }


        }

        @Override
        public void enterNegated_expression(SquigglyExpressionParser.Negated_expressionContext ctx) {
            negated = true;
        }

        @Override
        public void exitNegated_expression(SquigglyExpressionParser.Negated_expressionContext ctx) {
            if (negated) {
                current.negated = true;
            }
            negated = false;
        }

        @Override
        public void enterField(SquigglyExpressionParser.FieldContext ctx) {
            super.enterField(ctx);
        }

        @Override
        public void exitField(SquigglyExpressionParser.FieldContext ctx) {
            if (dotPath > 1) {
                current.squiggly = true;
                current = current.newChild();
            }

            if (dotPath > 0) {
                dotPath++;
            }


            current.addName(name);
        }

        @Override
        public void enterExact_field(SquigglyExpressionParser.Exact_fieldContext ctx) {
            name = new ExactName(ctx.getText());
        }

        @Override
        public void enterRegex_flag(SquigglyExpressionParser.Regex_flagContext ctx) {
            if (regexFlags == null) {
                regexFlags = new HashSet<>(10);
            }

            regexFlags.add(ctx.getText());
        }

        @Override
        public void enterRegex_pattern(SquigglyExpressionParser.Regex_patternContext ctx) {
            regexPattern = ctx.getText();
        }

        @Override
        public void exitRegex_field(SquigglyExpressionParser.Regex_fieldContext ctx) {
            name = new RegexName(regexPattern, regexFlags);
            regexPattern = null;
            regexFlags = null;
        }

        @Override
        public void enterWildcard_field(SquigglyExpressionParser.Wildcard_fieldContext ctx) {
            name = new WildcardName(ctx.getText());
        }

        @Override
        public void enterWildcard_shallow_field(SquigglyExpressionParser.Wildcard_shallow_fieldContext ctx) {
            name = AnyShallowName.get();
        }

        @Override
        public void enterDeep(SquigglyExpressionParser.DeepContext ctx) {
            current.addName(AnyDeepName.get());
        }

        public List<SquigglyNode> getNodes() {
            return parent.getSquigglyChildNodes(null);
        }
    }

    private class MutableNode {
        private List<SquigglyName> names;
        private boolean negated;
        private boolean squiggly;
        private boolean emptyNested;
        private List<MutableNode> children;
        private MutableNode parent;
        private MutableNode returnParent;
        public boolean nested;

        public List<SquigglyNode> toSquigglyNodes(SquigglyNode parentNode) {
            if (names == null || names.isEmpty()) {
                throw new IllegalArgumentException("No Names specified");
            }

            List<SquigglyNode> nodes;

            if (names.size() == 1) {
                nodes = Collections.singletonList(toSquigglyNode(names.get(0), parentNode));
            } else {
                nodes = new ArrayList<>(names.size());

                for (SquigglyName name : names) {
                    nodes.add(toSquigglyNode(name, parentNode));
                }
            }

            return nodes;
        }

        public SquigglyNode toSquigglyNode(SquigglyName name, SquigglyNode parentNode) {
            SquigglyNode node;

            if (children == null || children.isEmpty()) {
                node = newSquigglyNode(name, parentNode, Collections.<SquigglyNode>emptyList());
            } else {
                List<SquigglyNode> childNodes = new ArrayList<>(children.size());
                node = newSquigglyNode(name, parentNode, childNodes);
                addSquigglyChildNodes(node, childNodes);
            }

            return node;

        }

        private SquigglyNode newSquigglyNode(SquigglyName name, SquigglyNode parentNode, List<SquigglyNode> childNodes) {
            return new SquigglyNode(name, parentNode, childNodes, negated, squiggly, emptyNested);
        }

        @SuppressWarnings("SameParameterValue")
        private List<SquigglyNode> getSquigglyChildNodes(SquigglyNode parentNode) {
            if (children == null || children.isEmpty()) {
                return Collections.emptyList();
            }

            List<SquigglyNode> childNodes = new ArrayList<>();
            addSquigglyChildNodes(parentNode, childNodes);

            return childNodes;
        }

        private void addSquigglyChildNodes(SquigglyNode parentNode, List<SquigglyNode> childNodes) {
            boolean allNegated = true;

            for (MutableNode child : children) {
                childNodes.addAll(child.toSquigglyNodes(parentNode));

                if (allNegated && !child.negated) {
                    allNegated = false;
                }
            }

            if (allNegated) {
                childNodes.add(newSquigglyNode(new ExactName(PropertyView.BASE_VIEW), parentNode, Collections.<SquigglyNode>emptyList()));
            }
        }

        @SuppressWarnings("UnusedReturnValue")
        public MutableNode addName(SquigglyName name) {
            if (names == null) {
                names = new ArrayList<>();
            }

            names.add(name);
            return this;
        }

        public MutableNode withParent(MutableNode parent) {
            this.parent = parent;
            return this;
        }

        public MutableNode newChild() {
            MutableNode child = new MutableNode()
                    .withParent(this);

            if (children == null) {
                children = new ArrayList<>();
            }

            children.add(child);

            return child;
        }
    }


}

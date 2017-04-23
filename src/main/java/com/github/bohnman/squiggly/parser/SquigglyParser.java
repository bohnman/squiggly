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
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionBaseVisitor;
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionLexer;
import com.github.bohnman.squiggly.parser.antlr4.SquigglyExpressionParser;
import com.github.bohnman.squiggly.util.antlr4.ThrowingErrorListener;
import com.github.bohnman.squiggly.view.PropertyView;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import net.jcip.annotations.ThreadSafe;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

        Visiter visiter = new Visiter();
        List<SquigglyNode> nodes = visiter.visit(parser.parse());

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

    private class Visiter extends SquigglyExpressionBaseVisitor<List<SquigglyNode>> {
        MutableNode root = new MutableNode(new ExactName("root"));

        @Override
        public List<SquigglyNode> visitParse(SquigglyExpressionParser.ParseContext ctx) {
            handleExpressionList(ctx.expression_list(), root);
            return root.getSquigglyChildNodes(null);
        }

        private void handleExpressionList(SquigglyExpressionParser.Expression_listContext ctx, MutableNode parent) {
            for (SquigglyExpressionParser.ExpressionContext expressionContext : ctx.expression()) {
                handleExpression(expressionContext, parent);
            }
        }

        private void handleExpression(SquigglyExpressionParser.ExpressionContext ctx, MutableNode parent) {

            if (ctx.negated_expression() != null) {
                handleNegatedExpression(ctx.negated_expression(), parent);
                return;
            }

            List<SquigglyName> names;

            if (ctx.field() != null) {
                names = Collections.singletonList(createName(ctx.field()));
            } else if (ctx.dot_path() != null) {
                parent.squiggly = true;
                for (int i = 0; i < ctx.dot_path().field().size() - 1; i++) {
                    parent = parent.addChild(new MutableNode(createName(ctx.dot_path().field(i))));
                    parent.squiggly = true;
                }
                names = Collections.singletonList(createName(ctx.dot_path().field().get(ctx.dot_path().field().size() - 1)));
            } else if (ctx.field_list() != null) {
                names = new ArrayList<>(ctx.field_list().field().size());
                for (SquigglyExpressionParser.FieldContext fieldContext : ctx.field_list().field()) {
                    names.add(createName(fieldContext));
                }
            } else if (ctx.deep() != null) {
                names = Collections.singletonList((SquigglyName) AnyDeepName.get());
            } else {
                names = Collections.emptyList();
            }


            for (SquigglyName name : names) {
                MutableNode node = parent.addChild(new MutableNode(name));

                if (ctx.empty_nested_expression() != null) {
                    node.emptyNested = true;
                } else if (ctx.nested_expression() != null) {
                    node.squiggly = true;
                    handleExpressionList(ctx.nested_expression().expression_list(), node);
                }
            }

        }

        private SquigglyName createName(SquigglyExpressionParser.FieldContext ctx) {
            SquigglyName name;

            if (ctx.exact_field() != null) {
                name = new ExactName(ctx.getText());
            } else if (ctx.wildcard_field() != null) {
                name = new WildcardName(ctx.getText());
            } else if (ctx.regex_field() != null) {
                String regexPattern = ctx.regex_field().regex_pattern().getText();
                Set<String> regexFlags = new HashSet<>(ctx.regex_field().regex_flag().size());

                for (SquigglyExpressionParser.Regex_flagContext regex_flagContext : ctx.regex_field().regex_flag()) {
                    regexFlags.add(regex_flagContext.getText());
                }

                name = new RegexName(regexPattern, regexFlags);
            } else if (ctx.wildcard_shallow_field() != null) {
                name = AnyShallowName.get();
            } else {
                throw new IllegalArgumentException("Unhandle field: " + ctx.getText());
            }

            return name;
        }


        private void handleNegatedExpression(SquigglyExpressionParser.Negated_expressionContext ctx, MutableNode parent) {
            if (ctx.field() != null) {
                parent.addChild(new MutableNode(createName(ctx.field()), true));
            } else if (ctx.dot_path() != null) {
                for (SquigglyExpressionParser.FieldContext fieldContext : ctx.dot_path().field()) {
                    parent.squiggly = true;
                    parent = parent.addChild(new MutableNode(createName(fieldContext)));
                }

                parent.negated = true;
            }
        }

    }

    private class MutableNode {
        private SquigglyName name;
        private boolean negated;
        private boolean squiggly;
        private boolean emptyNested;
        private Map<String, MutableNode> children;
        private MutableNode parent;
        private MutableNode returnParent;
        public boolean nested;

        public MutableNode(SquigglyName name) {
            this.name = name;
        }

        public MutableNode(SquigglyName name, boolean negated) {
            this.name = name;
            this.negated = negated;
        }

        public SquigglyNode toSquigglyNode(SquigglyNode parentNode) {
            if (name == null) {
                throw new IllegalArgumentException("No Names specified");
            }

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

            for (MutableNode child : children.values()) {
                childNodes.add(child.toSquigglyNode(parentNode));

                if (allNegated && !child.negated) {
                    allNegated = false;
                }
            }

            if (allNegated) {
                childNodes.add(newSquigglyNode(new ExactName(PropertyView.BASE_VIEW), parentNode, Collections.<SquigglyNode>emptyList()));
            }
        }

        @SuppressWarnings("UnusedReturnValue")
        public MutableNode withName(SquigglyName name) {
            this.name = name;
            return this;
        }

        public MutableNode withParent(MutableNode parent) {
            this.parent = parent;
            return this;
        }

        public MutableNode addChild(MutableNode child) {
            child.parent = this;

            if (children == null) {
                children = new LinkedHashMap<>();
            }

            String name = child.name.getName();
            MutableNode existing = children.get(name);

            if (existing != null) {
                if (child.children != null) {
                    for (MutableNode otherChild : child.children.values()) {
                        otherChild.parent = this;
                    }

                    if (existing.children == null) {
                        existing.children = child.children;
                    } else {
                        existing.children.putAll(child.children);
                    }

                    existing.children.putAll(child.children);
                }


                existing.squiggly = existing.squiggly || child.squiggly;
                existing.emptyNested = existing.emptyNested && child.emptyNested;
                return existing;
            }

            children.put(name, child);

            return child;
        }
    }


}

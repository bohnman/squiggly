package com.github.bohnman.squiggly.parser;

import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.Lists;
import net.jcip.annotations.ThreadSafe;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

/**
 * The parser takes a filter expression and compiles it to an Abstract Syntax Tree (AST).  In this parser's case, the
 * tree doesn't have a root node but rather just returns top level nodes.
 */
@ThreadSafe
public class SquigglyParser {

    // Caches parsed filter expressions
    private static final Cache<String, List<SquigglyNode>> CACHE;

    static {
        int maxSize = SquigglyConfig.getParserNodeCacheMaxSize();
        CACHE = CacheBuilder.newBuilder().maximumSize(maxSize).build();
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

        // do the actual parsing
        List<SquigglyNode> nodes = Lists.newArrayList();
        parse(tokenize(filter, "{},"), 0, null, nodes, false);

        List<SquigglyNode> unmodifiableNodes = Collections.unmodifiableList(nodes);
        CACHE.put(filter, unmodifiableNodes);

        return unmodifiableNodes;
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

    // Do the parsing.
    @SuppressWarnings({"IfCanBeSwitch", "StatementWithEmptyBody"})
    private int parse(List<String> tokens, int startIndex, SquigglyNode parent, List<SquigglyNode> nodes, boolean parentSquiggly) {
        List<SquigglyNode> nodeStack = Lists.newArrayList();
        List<List<SquigglyNode>> childrenStack = Lists.newArrayList();
        int currentIndex;


        // Loop through the tokens, creating squiggly nodes
        for (currentIndex = startIndex; currentIndex < tokens.size(); currentIndex++) {
            String token = tokens.get(currentIndex);

            if (token.equals("{")) {
                if (nodeStack.isEmpty()) {
                    throw new IllegalStateException("Can't start { when there's no current node");
                }

                // parse the nexted filter
                if (nodeStack.size() >= 1) {
                    int newIndex = parse(tokens, currentIndex + 1, nodeStack.get(0), childrenStack.get(0), true);

                    if (nodeStack.size() > 1) {
                        for (int i = 1; i < nodeStack.size(); i++) {
                            parse(tokens, currentIndex + 1, nodeStack.get(i), childrenStack.get(i), true);
                        }
                    }

                    currentIndex = newIndex;
                }


            } else if (token.equals("}")) {
                // end the nested squiggly

                if (!parentSquiggly) {
                    throw new IllegalStateException("found '}' without a starting '{'.");
                }

                break;
            } else if (token.equals(",")) {
                // start another node
            } else {

                // clear out the old stacks
                nodeStack.clear();
                childrenStack.clear();

                // handle the case of an OR expression
                String[] names = StringUtils.split(token, '|');

                boolean nextTokenIsSquiggly = (currentIndex + 1 < tokens.size()) && StringUtils.equals(tokens.get(currentIndex + 1), "{");

                for (String name : names) {
                    if (StringUtils.isEmpty(name)) {
                        throw new IllegalStateException("Empty name");
                    }

                    List<SquigglyNode> currentChildren = Lists.newArrayList();
                    childrenStack.add(currentChildren);
                    SquigglyNode currentNode = new SquigglyNode(name, parent, currentChildren, nextTokenIsSquiggly);
                    nodeStack.add(currentNode);
                    nodes.add(currentNode);
                }
            }
        }

        return currentIndex;
    }
}

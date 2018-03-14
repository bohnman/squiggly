package com.github.bohnman.squiggly.cli;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.github.bohnman.core.io.OutputStreamWrapper;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.cli.config.RunnerConfig;
import com.github.bohnman.squiggly.cli.printer.SyntaxHighlighter;
import com.github.bohnman.squiggly.cli.printer.SyntaxHighlightingJsonGenerator;
import com.github.bohnman.squiggly.cli.printer.SyntaxHighlightingPrettyPrinter;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.function.functions.CoreJsonNodeFunctions;
import com.github.bohnman.squiggly.jackson.Squiggly;
import com.github.bohnman.squiggly.jackson.json.JacksonJsonNode;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.List;

public class Runner implements Runnable {

    private final ObjectMapper mapper;
    private final RunnerConfig config;
    private final Squiggly squiggly;
    private final SyntaxHighlighter syntaxHighlighter;


    public Runner(String... args) {
        this.config = new RunnerConfig(args);
        this.mapper = buildObjectMapper();
        this.squiggly = buildSquiggly();
        this.syntaxHighlighter = config.isColoredOutput() ? new SyntaxHighlighter(squiggly.getConfig()) : null;
    }

    private ObjectMapper buildObjectMapper() {
        ObjectMapper mapper = new ObjectMapper();

        if (!config.isCompact()) {
            mapper.enable(SerializationFeature.INDENT_OUTPUT);
            char indentCh = config.isTab() ? '\t' : ' ';
            String indent = CoreStrings.repeat(indentCh, config.getIndent());
            DefaultPrettyPrinter.Indenter indenter = new DefaultIndenter(indent, DefaultIndenter.SYS_LF);
            DefaultPrettyPrinter printer = syntaxHighlighter == null ? new DefaultPrettyPrinter() : new SyntaxHighlightingPrettyPrinter(syntaxHighlighter);
            mapper.setDefaultPrettyPrinter(printer);
        }

        if (config.isSortKeys()) {
            mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
        }

        return mapper;
    }

    private Squiggly buildSquiggly() {
        Squiggly.Builder builder = Squiggly.builder();
        config.getVariables().forEach(builder::variable);
        builder.config(new SquigglyConfig(config.getConfigSource()));

        return builder.build();
    }

    @Override
    public final void run() {
        try {
            doRun();
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    private void doRun() throws Throwable {
        List<JsonNode> nodes = config.isSlurp() ? new ArrayList<>() : null;

        if (config.isNullInput()) {
            apply(mapper.getNodeFactory().nullNode(), nodes);
        } else if (config.getFiles().isEmpty()) {
            read(System.in, nodes);
        } else {
            for (String file : config.getFiles()) {
                try (FileInputStream in = new FileInputStream(file)) {
                    read(in, nodes);
                }
            }
        }

        if (nodes != null) {
            ArrayNode arrayNode = mapper.getNodeFactory().arrayNode();
            nodes.forEach(arrayNode::add);
            write(arrayNode);
        }
    }

    private void read(InputStream in, List<JsonNode> nodes) {

        try (Reader reader = readerFor(in)) {
            JsonParser parser = mapper.getFactory().createParser(reader);

            while (!parser.isClosed()) {
                JsonNode tree = parser.readValueAsTree();

                if (tree == null) {
                    continue;
                }

                apply(tree, nodes);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private void apply(JsonNode tree, List<JsonNode> nodes) throws IOException {
        CoreJsonNode<JsonNode> result = squiggly.apply(new JacksonJsonNode(tree), config.getFilter());

        if (config.isFlatten()) {
            result = CoreJsonNodeFunctions.flatten(result);
        }

        JsonNode rawResult = result.getRawNode();

        if (config.isExpand() && rawResult.isArray()) {
            for (int i = 0; i < rawResult.size(); i++) {
                handleResult(rawResult.get(i), nodes);
            }
        } else {
            handleResult(rawResult, nodes);
        }
    }

    private void handleResult(JsonNode result, List<JsonNode> nodes) throws IOException {
        if (nodes == null) {
            write(result);
        } else {
            nodes.add(result);
        }
    }

    private void write(JsonNode node) throws IOException {
        PrintStream out = System.out;

        if (node.isTextual() && config.isRawOutput() && !config.isSlurp()) {
            if (config.isJoinOutput()) {
                out.print(node.asText());
            } else {
                out.println(node.asText());
            }
        } else {
            write(node, out);

            if (!config.isJoinOutput()) {
                out.println();
            }
        }
    }

    private void write(JsonNode tree, PrintStream out) throws IOException {
        JsonGenerator jgen = createGenerator(out);

        if (!config.isCompact()) {
            jgen.setPrettyPrinter(mapper.getSerializationConfig().getDefaultPrettyPrinter());
        }
        mapper.writeValue(jgen, tree);
    }

    private JsonGenerator createGenerator(PrintStream out) throws IOException {
        JsonGenerator jgen = mapper.getFactory().createGenerator(new PreventCloseOutputStream(out));

        if (syntaxHighlighter == null) {
            return jgen;
        }

        return new SyntaxHighlightingJsonGenerator(jgen, syntaxHighlighter);
    }

    private Reader readerFor(InputStream in) {
        return new BufferedReader(new InputStreamReader(in));
    }

    private class PreventCloseOutputStream extends OutputStreamWrapper {
        public PreventCloseOutputStream(OutputStream wrapped) {
            super(wrapped);
        }

        @Override
        public void close() {
        }
    }


    public static void main(String... args) {
        new Runner(args).run();
    }
}

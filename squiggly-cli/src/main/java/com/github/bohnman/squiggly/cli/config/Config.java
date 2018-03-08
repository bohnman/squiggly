package com.github.bohnman.squiggly.cli.config;

import com.beust.jcommander.DynamicParameter;
import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;
import com.beust.jcommander.ParameterException;
import com.github.bohnman.core.encoding.CoreCharsets;
import com.github.bohnman.core.io.CoreIo;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

@SuppressWarnings("FieldCanBeLocal")
public class Config {

    private static final int MAX_INDENT = 10;

    private final JCommander commander;

    @Parameter(names = {"-c", "--compact"}, description = "compact instead of pretty output")
    private boolean compact = false;

    @Parameter(description = "<squiggly-filter> [file...]")
    private List<String> files;

    private String filter;

    @Parameter(names = {"-f", "--from-file"}, description = "Read filter from the file rather than from a command line. You can also use '#' to make comments.")
    private File filterFile;

    @Parameter(names = {"-h", "--help"}, description = "Print Help", help = true)
    private boolean help = false;

    @Parameter(names = "--indent", description = "number of spaces/tabs to use (no more than 10)")
    private Integer indent = null;

    @Parameter(names = {"-n", "--null-input"}, description = "use null instead of reading input")
    private boolean nullInput;

    @Parameter(names = {"-r", "--raw-output"}, description = "if result is a string, don't print quotes")
    private boolean rawOutput;

    @Parameter(names = {"-s", "--sort-keys"}, description = "sort keys")
    private boolean sortKeys;

    @Parameter(names = "--tab", description = "indent output with tab instead of spaces")
    private boolean tab;

    @DynamicParameter(names = "-V", description = "sets a variable (eg. -Vfoo=bar)")
    private Map<String, String> variables = new HashMap<>();

    public Config(String... args) {
        System.out.println("ARGS: " + Arrays.toString(args));
        commander = JCommander.newBuilder()
                .addObject(this)
                .build();

        commander.setProgramName("squiggly");

        try {
            commander.parse(args);
            init();
        } catch (ParameterException e) {
            System.err.println("Error: " + e.getMessage());
            printUsage();
            System.exit(0);
        }

        if (help) {
            printUsage();
            System.exit(0);
        }
    }

    private void init() {
        if (files == null) {
            files = Collections.emptyList();
        }

        if (filterFile == null) {
            if (files.isEmpty()) throw new ParameterException("Either -f or first argument must be specified.");
            filter = files.get(0);
            files = files.size() == 1 ? Collections.emptyList() : files.subList(1, files.size());
        } else {
            filter = readFilter(filterFile.getAbsolutePath());
        }

        if (indent == null) {
            indent = tab ? 1 : 2;
        }

        if (indent < 0 || indent > MAX_INDENT) {
            throw new ParameterException(String.format("indent must be >= 0 and <= %s", MAX_INDENT));
        }

        System.out.println(this);
    }

    private String readFilter(String path) {
        String input = CoreIo.toString(path, CoreCharsets.UTF_8);
        return Pattern.compile("^\\s*#.*$", Pattern.MULTILINE)
                .matcher(input)
                .replaceAll("")
                .trim();

    }

    public List<String> getFiles() {
        return files;
    }

    public String getFilter() {
        return filter;
    }

    public boolean isHelp() {
        return help;
    }

    public boolean isCompact() {
        return compact;
    }

    public int getIndent() {
        return indent;
    }

    public boolean isNullInput() {
        return nullInput;
    }

    public boolean isRawOutput() {
        return rawOutput;
    }

    public boolean isSortKeys() {
        return sortKeys;
    }

    public boolean isTab() {
        return tab;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void printUsage() {
        commander.usage();
    }

    public static void main(String[] args) {
        System.out.println(Pattern.compile("^\\s+#.*$", Pattern.MULTILINE)
                .matcher(" # foo \n bar")
                .replaceAll(""));
    }

    @Override
    public String toString() {
        return "Config{" +
                "compact=" + compact +
                ", files=" + files +
                ", filter='" + filter + '\'' +
                ", filterFile=" + filterFile +
                ", help=" + help +
                ", indent=" + indent +
                ", nullInput=" + nullInput +
                ", rawOutput=" + rawOutput +
                ", sortKeys=" + sortKeys +
                ", tab=" + tab +
                ", variables=" + variables +
                '}';
    }
}

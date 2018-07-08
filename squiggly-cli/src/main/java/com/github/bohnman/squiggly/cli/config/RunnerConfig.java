package com.github.bohnman.squiggly.cli.config;

import com.beust.jcommander.DynamicParameter;
import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;
import com.beust.jcommander.ParameterException;
import com.github.bohnman.core.encoding.CoreCharsets;
import com.github.bohnman.core.io.CoreIo;
import com.github.bohnman.core.vcs.GitInfo;
import com.github.bohnman.squiggly.core.config.source.PropertiesConfigSource;
import com.github.bohnman.squiggly.core.config.source.SquigglyConfigSource;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Configuration used by the CLI runner.
 */
@SuppressWarnings("FieldCanBeLocal")
public class RunnerConfig {

    private static final int MAX_INDENT = 10;

    private String baseSquigglyPath = System.getProperty("user.home") + File.separatorChar + ".squiggly";

    private final JCommander commander;

    /**
     * Force colored output regardless of piping. Default is false.
     */
    @Parameter(names = {"-C", "--color-output"}, description = "Force colored output")
    private boolean coloredOutput;

    /**
     * Force monochrome color output. Default is false.
     */
    @Parameter(names = {"-M", "--monochrome-output"}, description = "Force monochrome output")
    private boolean monochromeOutput;

    /**
     * Output result on a single line. Default is false.
     */
    @Parameter(names = {"-c", "--compact"}, description = "compact instead of pretty output")
    private boolean compact = false;

    /**
     * Expand arrays into separate lines of output.  This makes the behavior similar to jq. Default is false.
     */
    @Parameter(names = {"-E", "--expand"}, description = "expand arrays into separate output")
    private boolean expand = false;

    private SquigglyConfigSource configSource;

    /**
     * The files containing json input.
     */
    @Parameter(description = "<squiggly-filter> [file...]")
    private List<String> files;

    private String filter;

    /**
     * Read filter from file instead of directly from the terminal.
     */
    @Parameter(names = {"-f", "--from-file"}, description = "Read filter from the file rather than from a command line. You can also use '#' to make comments.")
    private File filterFile;


    @Parameter(names = {"--F", "--flatten"}, description = "Flatten nested arrays")
    private boolean flatten;

    /**
     * Max depth to flatten.  Default is no max.
     *
     * @see #flatten
     */
    @Parameter(names = {"--flatten-depth"}, description = "Max depth to flatten")
    private int flattenDepth;

    /**
     * Print Usage.
     */
    @Parameter(names = {"-h", "--help"}, description = "Print Help", help = true)
    private boolean help = false;

    /**
     * Number of spaces or tabs to use.  There is a hard limit of {@value MAX_INDENT}.
     *
     * @see #tab
     */
    @Parameter(names = "--indent", description = "number of spaces/tabs to use (no more than 10)")
    private Integer indent = null;

    /**
     * Don't print a newline after each record.  Default is false.
     */
    @Parameter(names = {"-j", "--join-output"}, description = "don't print a newline after each record")
    private boolean joinOutput;

    /**
     * Use a null value instead of reading json input.  Useful for testing.  Default is false.
     */
    @Parameter(names = {"-n", "--null-input"}, description = "use null instead of reading input")
    private boolean nullInput;

    /**
     * If the result is a string, don't print the quotes. This can be useful for making jq filters talk to
     * non-json-based systems. Default is false.
     */
    @Parameter(names = {"-r", "--raw-output"}, description = "if result is a string, don't print quotes")
    private boolean rawOutput;

    /**
     * Reads all json into an array. Default is false.
     */
    @Parameter(names = {"-s", "--slurp"}, description = "read all input into an array")
    private boolean slurp;

    /**
     * Sorts map keys on all output.  Default is false.
     */
    @Parameter(names = {"-S", "--sort-keys"}, description = "sort keys")
    private boolean sortKeys;

    /**
     * Indent output with tabs instead of spaces. Default is false.
     */
    @Parameter(names = "--tab", description = "indent output with tabs instead of spaces")
    private boolean tab;

    /**
     * Indicates stdin is not being piped.  Default is false.  This is usually passed in from a shell script because
     * java cannot determine this by itself.
     */
    @Parameter(names = "--tty-in", description = "indicates input is not being piped", hidden = true)
    private boolean ttyIn;

    /**
     * Indicates stout is not being piped.  Default is false.  This is usually passed in from a shell script because
     * java cannot determine this by itself.
     */
    @Parameter(names = "--tty-out", description = "indicates output is not being piped", hidden = true)
    private boolean ttyOut;

    /**
     * Sets variables that can be used in squiggly filter expressions.
     * <p>
     * For example:
     * <pre>
     *     -Vfoo=bar  // sets a variable called foo to the value bar which can be used in a squiggly filter.
     * </pre>
     */
    @DynamicParameter(names = "-V", description = "sets a variable (eg. -Vfoo=bar)")
    private Map<String, Object> variables = new HashMap<>();

    /**
     * Prints squiggly version.
     */
    @Parameter(names = "--version", description = "print version")
    private boolean version;

    public RunnerConfig(String... args) {
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
            System.exit(1);
        }


    }

    private void init() {
        if (help) {
            printUsage();
            System.exit(0);
        }

        if (version) {
            GitInfo info = GitInfo.forClasspathResource("squiggly/cli/git.properties");
            System.out.println("Version      : " + info.getVersionText());
            System.out.println("Build Author : " + info.getBuildAuthor());
            System.out.println("Build Date   : " + info.getBuildDateTime());
            System.out.println("Commit Id    : " + info.getAbbreviatedCommitId());
            System.exit(0);
        }

        initFilterAndFiles();
        initIndent();
        initProperties();
        initVariables();

//        System.out.println(this);
    }

    private void initFilterAndFiles() {
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

        if (files.isEmpty() && isForceFiles()) {
            throw new ParameterException("At least 1 file must be specified.");
        }
    }

    private void initProperties() {
        Properties properties = new Properties();

        File propsFile = new File(baseSquigglyPath + File.separatorChar + "config");

        if (propsFile.exists()) {
            try {
                properties.load(new FileInputStream(propsFile));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        configSource = new PropertiesConfigSource(propsFile.getAbsolutePath(), properties);

    }

    private void initVariables() {
        Properties properties = new Properties();

        File propsFile = new File(baseSquigglyPath + File.separatorChar + "variables");

        if (propsFile.exists()) {
            try {
                properties.load(new FileInputStream(propsFile));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        properties.forEach((key, value) -> variables.putIfAbsent(key.toString(), value));
    }

    private void initIndent() {
        if (indent == null) {
            indent = tab ? 1 : 2;
        }

        if (indent < 0 || indent > MAX_INDENT) {
            throw new ParameterException(String.format("indent must be >= 0 and <= %s", MAX_INDENT));
        }
    }

    private String readFilter(String path) {
        String input = CoreIo.toString(path, CoreCharsets.UTF_8);
        return Pattern.compile("^\\s*#.*$", Pattern.MULTILINE)
                .matcher(input)
                .replaceAll("")
                .trim();

    }

    private boolean isForceFiles() {
        if (ttyIn && ttyOut) {
            return true;
        }

        return false;
    }

    public boolean isColoredOutput() {
        if (monochromeOutput) {
            return false;
        }

        if (coloredOutput || ttyOut) {
            return true;
        }

        return System.console() != null;
    }

    public JCommander getCommander() {
        return commander;
    }

    public boolean isExpand() {
        return expand;
    }

    public List<String> getFiles() {
        return files;
    }

    public String getFilter() {
        return filter;
    }

    public boolean isFlatten() {
        return flatten;
    }

    public int getFlattenDepth() {
        return flattenDepth;
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

    public boolean isJoinOutput() {
        return joinOutput;
    }

    public boolean isNullInput() {
        return nullInput;
    }

    public SquigglyConfigSource getConfigSource() {
        return configSource;
    }

    public boolean isRawOutput() {
        return rawOutput;
    }

    public String getBaseSquigglyPath() {
        return baseSquigglyPath;
    }

    public boolean isSlurp() {
        return slurp;
    }

    public boolean isSortKeys() {
        return sortKeys;
    }

    public boolean isTab() {
        return tab;
    }

    public Map<String, Object> getVariables() {
        return variables;
    }

    public void printUsage() {
        commander.usage();
    }

    @Override
    public String toString() {
        return "RunnerConfig{" +
                "baseSquigglyPath='" + baseSquigglyPath + '\'' +
                ", coloredOutput=" + coloredOutput +
                ", monochromeOutput=" + monochromeOutput +
                ", compact=" + compact +
                ", configSource=" + configSource +
                ", expand=" + expand +
                ", files=" + files +
                ", filter='" + filter + '\'' +
                ", filterFile=" + filterFile +
                ", flatten=" + flatten +
                ", flattenDepth=" + flattenDepth +
                ", help=" + help +
                ", indent=" + indent +
                ", joinOutput=" + joinOutput +
                ", nullInput=" + nullInput +
                ", rawOutput=" + rawOutput +
                ", slurp=" + slurp +
                ", sortKeys=" + sortKeys +
                ", tab=" + tab +
                ", ttyIn=" + ttyIn +
                ", ttyOut=" + ttyOut +
                ", variables=" + variables +
                '}';
    }
}

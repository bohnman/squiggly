package com.github.bohnman.squiggly.cli.printer;

/**
 * Represents Ansi colors and styles safe for command line output.
 *
 * @see <a href="http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html">http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html</a>
 */
public enum Ansi {

    // Color code strings from:
    // http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html

    RESET("\u001B[0m"),

    HIGH_INTENSITY("\u001B[1m"),
    LOW_INTENSITY("\u001B[2m"),

    ITALIC("\u001B[3m"),
    UNDERLINE("\u001B[4m"),
    BLINK("\u001B[5m"),
    RAPID_BLINK("\u001B[6m"),
    REVERSE_VIDEO("\u001B[7m"),
    INVISIBLE_TEXT("\u001B[8m"),

    BLACK("\u001B[30m"),
    RED("\u001B[31m"),
    GREEN("\u001B[32m"),
    YELLOW("\u001B[33m"),
    BLUE("\u001B[34m"),
    MAGENTA("\u001B[35m"),
    CYAN("\u001B[36m"),
    WHITE("\u001B[37m"),

    BACKGROUND_BLACK("\u001B[40m"),
    BACKGROUND_RED("\u001B[41m"),
    BACKGROUND_GREEN("\u001B[42m"),
    BACKGROUND_YELLOW("\u001B[43m"),
    BACKGROUND_BLUE("\u001B[44m"),
    BACKGROUND_MAGENTA("\u001B[45m"),
    BACKGROUND_CYAN("\u001B[46m"),
    BACKGROUND_WHITE("\u001B[47m"),;

    private final String code;

    Ansi(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}

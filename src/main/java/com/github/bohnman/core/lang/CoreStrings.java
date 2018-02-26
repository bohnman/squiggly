package com.github.bohnman.core.lang;

import com.github.bohnman.core.lang.array.CoreArrays;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;

@SuppressWarnings("SameParameterValue")
public class CoreStrings {

    private static final CharSequenceTranslator UNESCAPE_JAVA =
            new AggregateTranslator(
                    new OctalUnescaper(),     // .between('\1', '\377'),
                    new UnicodeUnescaper(),
                    new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_UNESCAPE()),
                    new LookupTranslator(
                            new String[][]{
                                    {"\\\\", "\\"},
                                    {"\\\"", "\""},
                                    {"\\'", "'"},
                                    {"\\", ""}
                            })
            );


    public static String capitalize(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }

        final char firstChar = str.charAt(0);
        if (Character.isTitleCase(firstChar)) {
            // already capitalized
            return str;
        }

        return new StringBuilder(strLen)
                .append(Character.toTitleCase(firstChar))
                .append(str.substring(1))
                .toString();
    }

    public static <T extends CharSequence> T defaultIfEmpty(final T str, final T defaultStr) {
        return isEmpty(str) ? defaultStr : str;
    }

    public static boolean isEmpty(final CharSequence cs) {
        return cs == null || cs.length() == 0;
    }

    public static boolean isNotEmpty(final CharSequence cs) {
        return cs != null && cs.length() != 0;
    }

    public static String remove(String str, String remove) {
        if (isEmpty(str) || isEmpty(remove)) {
            return str;
        }
        return replace(str, remove, "", -1);
    }

    public static String lower(String str) {
        return str == null ? null : str.toLowerCase();
    }

    public static String replace(String str, String search, String replace) {
        return replace(str, search, replace, -1);
    }

    public static String replace(String str, String search, String replace, int max) {
        if (isEmpty(str) || isEmpty(search) || replace == null || max == 0) {
            return str;
        }
        int start = 0;
        int end = str.indexOf(search, start);
        if (end == -1) {
            return str;
        }
        final int replLength = search.length();
        int increase = replace.length() - replLength;
        increase = increase < 0 ? 0 : increase;
        increase *= max < 0 ? 16 : max > 64 ? 64 : max;
        final StringBuilder buf = new StringBuilder(str.length() + increase);
        while (end != -1) {
            buf.append(str.substring(start, end)).append(replace);
            start = end + replLength;
            if (--max == 0) {
                break;
            }
            end = str.indexOf(search, start);
        }
        buf.append(str.substring(start));
        return buf.toString();
    }

    public static String replaceEach(String str, String[] search, String[] replace) {
        return replaceEach(str, search, replace, false, 0);
    }

    public static String reverse(String str) {
        if (str == null) {
            return null;
        }
        return new StringBuilder(str).reverse().toString();
    }

    public static String[] split(String str, String separator) {
        return splitByWholeSeparatorWorker(str, separator, -1, false);
    }

    public static String substring(String str, int start) {
        if (str == null) {
            return null;
        }

        // handle negatives, which means last n characters
        if (start < 0) {
            start = str.length() + start; // remember start is negative
        }

        if (start < 0) {
            start = 0;
        }
        if (start > str.length()) {
            return "";
        }

        return str.substring(start);
    }

    public static String substring(String str, int start, int end) {
        if (str == null) {
            return null;
        }

        // handle negatives
        if (end < 0) {
            end = str.length() + end; // remember end is negative
        }
        if (start < 0) {
            start = str.length() + start; // remember start is negative
        }

        // check length next
        if (end > str.length()) {
            end = str.length();
        }

        // if start is greater than end, return ""
        if (start > end) {
            return "";
        }

        if (start < 0) {
            start = 0;
        }
        if (end < 0) {
            end = 0;
        }

        return str.substring(start, end);
    }

    public static String trim(String str) {
        return str == null ? null : str.trim();
    }

    public static String trimToEmpty(String str) {
        return str == null ? "" : str.trim();
    }

    public static String unescapeEcmaScript(String str) {
        return UNESCAPE_JAVA.translate(str);
    }

    public static String upper(String str) {
        return str == null ? null : str.toUpperCase();
    }


    @SuppressWarnings("UnusedAssignment")
    private static String replaceEach(
            final String text, final String[] searchList, final String[] replacementList, final boolean repeat, final int timeToLive) {

        // mchyzer Performance note: This creates very few new objects (one major goal)
        // let me know if there are performance requests, we can create a harness to measure

        if (text == null || text.isEmpty() || searchList == null ||
                searchList.length == 0 || replacementList == null || replacementList.length == 0) {
            return text;
        }

        // if recursing, this shouldn't be less than 0
        if (timeToLive < 0) {
            throw new IllegalStateException("Aborting to protect against StackOverflowError - " +
                    "output of one loop is the input of another");
        }

        final int searchLength = searchList.length;
        final int replacementLength = replacementList.length;

        // make sure lengths are ok, these need to be equal
        if (searchLength != replacementLength) {
            throw new IllegalArgumentException("Search and Replace array lengths don't match: "
                    + searchLength
                    + " vs "
                    + replacementLength);
        }

        // keep track of which still have matches
        final boolean[] noMoreMatchesForReplIndex = new boolean[searchLength];

        // index on index that the match was found
        int textIndex = -1;
        int replaceIndex = -1;
        int tempIndex = -1;

        // index of replace array that will replace the search string found
        // NOTE: logic duplicated below START
        for (int i = 0; i < searchLength; i++) {
            if (noMoreMatchesForReplIndex[i] || searchList[i] == null ||
                    searchList[i].isEmpty() || replacementList[i] == null) {
                continue;
            }
            tempIndex = text.indexOf(searchList[i]);

            // see if we need to keep searching for this
            if (tempIndex == -1) {
                noMoreMatchesForReplIndex[i] = true;
            } else {
                if (textIndex == -1 || tempIndex < textIndex) {
                    textIndex = tempIndex;
                    replaceIndex = i;
                }
            }
        }
        // NOTE: logic mostly below END

        // no search strings found, we are done
        if (textIndex == -1) {
            return text;
        }

        int start = 0;

        // get a good guess on the size of the result buffer so it doesn't have to double if it goes over a bit
        int increase = 0;

        // count the replacement text elements that are larger than their corresponding text being replaced
        for (int i = 0; i < searchList.length; i++) {
            if (searchList[i] == null || replacementList[i] == null) {
                continue;
            }
            final int greater = replacementList[i].length() - searchList[i].length();
            if (greater > 0) {
                increase += 3 * greater; // assume 3 matches
            }
        }
        // have upper-bound at 20% increase, then let Java take over
        increase = Math.min(increase, text.length() / 5);

        final StringBuilder buf = new StringBuilder(text.length() + increase);

        while (textIndex != -1) {

            for (int i = start; i < textIndex; i++) {
                buf.append(text.charAt(i));
            }
            buf.append(replacementList[replaceIndex]);

            start = textIndex + searchList[replaceIndex].length();

            textIndex = -1;
            replaceIndex = -1;
            tempIndex = -1;
            // find the next earliest match
            // NOTE: logic mostly duplicated above START
            for (int i = 0; i < searchLength; i++) {
                if (noMoreMatchesForReplIndex[i] || searchList[i] == null ||
                        searchList[i].isEmpty() || replacementList[i] == null) {
                    continue;
                }
                tempIndex = text.indexOf(searchList[i], start);

                // see if we need to keep searching for this
                if (tempIndex == -1) {
                    noMoreMatchesForReplIndex[i] = true;
                } else {
                    if (textIndex == -1 || tempIndex < textIndex) {
                        textIndex = tempIndex;
                        replaceIndex = i;
                    }
                }
            }
            // NOTE: logic duplicated above END

        }
        final int textLength = text.length();
        for (int i = start; i < textLength; i++) {
            buf.append(text.charAt(i));
        }
        final String result = buf.toString();
        if (!repeat) {
            return result;
        }

        return replaceEach(result, searchList, replacementList, repeat, timeToLive - 1);
    }

    private static String[] splitByWholeSeparatorWorker(
            final String str, final String separator, final int max, final boolean preserveAllTokens) {
        if (str == null) {
            return null;
        }

        final int len = str.length();

        if (len == 0) {
            return CoreArrays.emptyStringArray();
        }

        if (separator == null || "".equals(separator)) {
            // Split on whitespace.
            return splitWorker(str, null, max, preserveAllTokens);
        }

        final int separatorLength = separator.length();

        final ArrayList<String> substrings = new ArrayList<>();
        int numberOfSubstrings = 0;
        int beg = 0;
        int end = 0;
        while (end < len) {
            end = str.indexOf(separator, beg);

            if (end > -1) {
                if (end > beg) {
                    numberOfSubstrings += 1;

                    if (numberOfSubstrings == max) {
                        end = len;
                        substrings.add(str.substring(beg));
                    } else {
                        // The following is OK, because String.substring( beg, end ) excludes
                        // the character at the position 'end'.
                        substrings.add(str.substring(beg, end));

                        // Set the starting point for the next search.
                        // The following is equivalent to beg = end + (separatorLength - 1) + 1,
                        // which is the right calculation:
                        beg = end + separatorLength;
                    }
                } else {
                    // We found a consecutive occurrence of the separator, so skip it.
                    if (preserveAllTokens) {
                        numberOfSubstrings += 1;
                        if (numberOfSubstrings == max) {
                            end = len;
                            substrings.add(str.substring(beg));
                        } else {
                            substrings.add("");
                        }
                    }
                    beg = end + separatorLength;
                }
            } else {
                // String.substring( beg ) goes from 'beg' to the end of the String.
                substrings.add(str.substring(beg));
                end = len;
            }
        }

        return substrings.toArray(new String[substrings.size()]);
    }

    private static String[] splitWorker(final String str, final String separatorChars, final int max, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)
        // Direct code is quicker than StringTokenizer.
        // Also, StringTokenizer uses isSpace() not isWhitespace()

        if (str == null) {
            return null;
        }
        final int len = str.length();
        if (len == 0) {
            return CoreArrays.emptyStringArray();
        }
        final List<String> list = new ArrayList<>();
        int sizePlus1 = 1;
        int i = 0, start = 0;
        boolean match = false;
        boolean lastMatch = false;
        if (separatorChars == null) {
            // Null separator means use whitespace
            while (i < len) {
                if (Character.isWhitespace(str.charAt(i))) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else if (separatorChars.length() == 1) {
            // Optimise 1 character case
            final char sep = separatorChars.charAt(0);
            while (i < len) {
                if (str.charAt(i) == sep) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else {
            // standard case
            while (i < len) {
                if (separatorChars.indexOf(str.charAt(i)) >= 0) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        }
        if (match || preserveAllTokens && lastMatch) {
            list.add(str.substring(start, i));
        }
        return list.toArray(new String[list.size()]);
    }

    /**
     * An API for translating text.
     * Its core use is to escape and unescape text. Because escaping and unescaping
     * is completely contextual, the API does not present two separate signatures.
     *
     * @version $Id: CharSequenceTranslator.java 1666535 2015-03-13 18:18:59Z britter $
     * @since 3.0
     */
    private static abstract class CharSequenceTranslator {

        static final char[] HEX_DIGITS = new char[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

        /**
         * Translate a set of codepoints, represented by an int index into a CharSequence,
         * into another set of codepoints. The number of codepoints consumed must be returned,
         * and the only IOExceptions thrown must be from interacting with the Writer so that
         * the top level API may reliably ignore StringWriter IOExceptions.
         *
         * @param input CharSequence that is being translated
         * @param index int representing the current point of translation
         * @param out   Writer to translate the text to
         * @return int count of codepoints consumed
         * @throws IOException if and only if the Writer produces an IOException
         */
        public abstract int translate(CharSequence input, int index, Writer out) throws IOException;

        /**
         * Helper for non-Writer usage.
         *
         * @param input CharSequence to be translated
         * @return String output of translation
         */
        public final String translate(final CharSequence input) {
            if (input == null) {
                return null;
            }
            try {
                final StringWriter writer = new StringWriter(input.length() * 2);
                translate(input, writer);
                return writer.toString();
            } catch (final IOException ioe) {
                // this should never ever happen while writing to a StringWriter
                throw new RuntimeException(ioe);
            }
        }

        /**
         * Translate an input onto a Writer. This is intentionally final as its algorithm is
         * tightly coupled with the abstract method of this class.
         *
         * @param input CharSequence that is being translated
         * @param out   Writer to translate the text to
         * @throws IOException if and only if the Writer produces an IOException
         */
        public final void translate(final CharSequence input, final Writer out) throws IOException {
            if (out == null) {
                throw new IllegalArgumentException("The Writer must not be null");
            }
            if (input == null) {
                return;
            }
            int pos = 0;
            final int len = input.length();
            while (pos < len) {
                final int consumed = translate(input, pos, out);
                if (consumed == 0) {
                    // inlined implementation of Character.toChars(Character.codePointAt(input, pos))
                    // avoids allocating temp char arrays and duplicate checks
                    char c1 = input.charAt(pos);
                    out.write(c1);
                    pos++;
                    if (Character.isHighSurrogate(c1) && pos < len) {
                        char c2 = input.charAt(pos);
                        if (Character.isLowSurrogate(c2)) {
                            out.write(c2);
                            pos++;
                        }
                    }
                    continue;
                }
                // contract with translators is that they have to understand codepoints
                // and they just took care of a surrogate pair
                for (int pt = 0; pt < consumed; pt++) {
                    pos += Character.charCount(Character.codePointAt(input, pos));
                }
            }
        }

        /**
         * Helper method to create a merger of this translator with another set of
         * translators. Useful in customizing the standard functionality.
         *
         * @param translators CharSequenceTranslator array of translators to merge with this one
         * @return CharSequenceTranslator merging this translator with the others
         */
        public final CharSequenceTranslator with(final CharSequenceTranslator... translators) {
            final CharSequenceTranslator[] newArray = new CharSequenceTranslator[translators.length + 1];
            newArray[0] = this;
            System.arraycopy(translators, 0, newArray, 1, translators.length);
            return new AggregateTranslator(newArray);
        }

        /**
         * <p>Returns an upper case hexadecimal <code>String</code> for the given
         * character.</p>
         *
         * @param codepoint The codepoint to convert.
         * @return An upper case hexadecimal <code>String</code>
         */
        private static String hex(final int codepoint) {
            return Integer.toHexString(codepoint).toUpperCase(Locale.ENGLISH);
        }

    }


    /**
     * Executes a sequence of translators one after the other. Execution ends whenever
     * the first translator consumes codepoints from the input.
     *
     * @version $Id: AggregateTranslator.java 1436770 2013-01-22 07:09:45Z ggregory $
     * @since 3.0
     */
    private static class AggregateTranslator extends CharSequenceTranslator {

        private final CharSequenceTranslator[] translators;

        /**
         * Specify the translators to be used at creation time.
         *
         * @param translators CharSequenceTranslator array to aggregate
         */
        public AggregateTranslator(final CharSequenceTranslator... translators) {
            this.translators = CoreArrays.clone(translators);
        }

        /**
         * The first translator to consume codepoints from the input is the 'winner'.
         * Execution stops with the number of consumed codepoints being returned.
         * {@inheritDoc}
         */
        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
            for (final CharSequenceTranslator translator : translators) {
                final int consumed = translator.translate(input, index, out);
                if (consumed != 0) {
                    return consumed;
                }
            }
            return 0;
        }

    }

    /**
     * Class holding various entity data for HTML and XML - generally for use with
     * the LookupTranslator.
     * All arrays are of length [*][2].
     *
     * @version $Id: EntityArrays.java 1436770 2013-01-22 07:09:45Z ggregory $
     * @since 3.0
     */
    private static class EntityArrays {

        /**
         * Mapping to escape <a href="https://secure.wikimedia.org/wikipedia/en/wiki/ISO/IEC_8859-1">ISO-8859-1</a>
         * characters to their named HTML 3.x equivalents.
         *
         * @return the mapping table
         */
        private static String[][] ISO8859_1_ESCAPE() {
            return ISO8859_1_ESCAPE.clone();
        }

        private static final String[][] ISO8859_1_ESCAPE = {
                {"\u00A0", "&nbsp;"}, // non-breaking space
                {"\u00A1", "&iexcl;"}, // inverted exclamation mark
                {"\u00A2", "&cent;"}, // cent sign
                {"\u00A3", "&pound;"}, // pound sign
                {"\u00A4", "&curren;"}, // currency sign
                {"\u00A5", "&yen;"}, // yen sign = yuan sign
                {"\u00A6", "&brvbar;"}, // broken bar = broken vertical bar
                {"\u00A7", "&sect;"}, // section sign
                {"\u00A8", "&uml;"}, // diaeresis = spacing diaeresis
                {"\u00A9", "&copy;"}, // � - copyright sign
                {"\u00AA", "&ordf;"}, // feminine ordinal indicator
                {"\u00AB", "&laquo;"}, // left-pointing double angle quotation mark = left pointing guillemet
                {"\u00AC", "&not;"}, // not sign
                {"\u00AD", "&shy;"}, // soft hyphen = discretionary hyphen
                {"\u00AE", "&reg;"}, // � - registered trademark sign
                {"\u00AF", "&macr;"}, // macron = spacing macron = overline = APL overbar
                {"\u00B0", "&deg;"}, // degree sign
                {"\u00B1", "&plusmn;"}, // plus-minus sign = plus-or-minus sign
                {"\u00B2", "&sup2;"}, // superscript two = superscript digit two = squared
                {"\u00B3", "&sup3;"}, // superscript three = superscript digit three = cubed
                {"\u00B4", "&acute;"}, // acute accent = spacing acute
                {"\u00B5", "&micro;"}, // micro sign
                {"\u00B6", "&para;"}, // pilcrow sign = paragraph sign
                {"\u00B7", "&middot;"}, // middle dot = Georgian comma = Greek middle dot
                {"\u00B8", "&cedil;"}, // cedilla = spacing cedilla
                {"\u00B9", "&sup1;"}, // superscript one = superscript digit one
                {"\u00BA", "&ordm;"}, // masculine ordinal indicator
                {"\u00BB", "&raquo;"}, // right-pointing double angle quotation mark = right pointing guillemet
                {"\u00BC", "&frac14;"}, // vulgar fraction one quarter = fraction one quarter
                {"\u00BD", "&frac12;"}, // vulgar fraction one half = fraction one half
                {"\u00BE", "&frac34;"}, // vulgar fraction three quarters = fraction three quarters
                {"\u00BF", "&iquest;"}, // inverted question mark = turned question mark
                {"\u00C0", "&Agrave;"}, // � - uppercase A, grave accent
                {"\u00C1", "&Aacute;"}, // � - uppercase A, acute accent
                {"\u00C2", "&Acirc;"}, // � - uppercase A, circumflex accent
                {"\u00C3", "&Atilde;"}, // � - uppercase A, tilde
                {"\u00C4", "&Auml;"}, // � - uppercase A, umlaut
                {"\u00C5", "&Aring;"}, // � - uppercase A, ring
                {"\u00C6", "&AElig;"}, // � - uppercase AE
                {"\u00C7", "&Ccedil;"}, // � - uppercase C, cedilla
                {"\u00C8", "&Egrave;"}, // � - uppercase E, grave accent
                {"\u00C9", "&Eacute;"}, // � - uppercase E, acute accent
                {"\u00CA", "&Ecirc;"}, // � - uppercase E, circumflex accent
                {"\u00CB", "&Euml;"}, // � - uppercase E, umlaut
                {"\u00CC", "&Igrave;"}, // � - uppercase I, grave accent
                {"\u00CD", "&Iacute;"}, // � - uppercase I, acute accent
                {"\u00CE", "&Icirc;"}, // � - uppercase I, circumflex accent
                {"\u00CF", "&Iuml;"}, // � - uppercase I, umlaut
                {"\u00D0", "&ETH;"}, // � - uppercase Eth, Icelandic
                {"\u00D1", "&Ntilde;"}, // � - uppercase N, tilde
                {"\u00D2", "&Ograve;"}, // � - uppercase O, grave accent
                {"\u00D3", "&Oacute;"}, // � - uppercase O, acute accent
                {"\u00D4", "&Ocirc;"}, // � - uppercase O, circumflex accent
                {"\u00D5", "&Otilde;"}, // � - uppercase O, tilde
                {"\u00D6", "&Ouml;"}, // � - uppercase O, umlaut
                {"\u00D7", "&times;"}, // multiplication sign
                {"\u00D8", "&Oslash;"}, // � - uppercase O, slash
                {"\u00D9", "&Ugrave;"}, // � - uppercase U, grave accent
                {"\u00DA", "&Uacute;"}, // � - uppercase U, acute accent
                {"\u00DB", "&Ucirc;"}, // � - uppercase U, circumflex accent
                {"\u00DC", "&Uuml;"}, // � - uppercase U, umlaut
                {"\u00DD", "&Yacute;"}, // � - uppercase Y, acute accent
                {"\u00DE", "&THORN;"}, // � - uppercase THORN, Icelandic
                {"\u00DF", "&szlig;"}, // � - lowercase sharps, German
                {"\u00E0", "&agrave;"}, // � - lowercase a, grave accent
                {"\u00E1", "&aacute;"}, // � - lowercase a, acute accent
                {"\u00E2", "&acirc;"}, // � - lowercase a, circumflex accent
                {"\u00E3", "&atilde;"}, // � - lowercase a, tilde
                {"\u00E4", "&auml;"}, // � - lowercase a, umlaut
                {"\u00E5", "&aring;"}, // � - lowercase a, ring
                {"\u00E6", "&aelig;"}, // � - lowercase ae
                {"\u00E7", "&ccedil;"}, // � - lowercase c, cedilla
                {"\u00E8", "&egrave;"}, // � - lowercase e, grave accent
                {"\u00E9", "&eacute;"}, // � - lowercase e, acute accent
                {"\u00EA", "&ecirc;"}, // � - lowercase e, circumflex accent
                {"\u00EB", "&euml;"}, // � - lowercase e, umlaut
                {"\u00EC", "&igrave;"}, // � - lowercase i, grave accent
                {"\u00ED", "&iacute;"}, // � - lowercase i, acute accent
                {"\u00EE", "&icirc;"}, // � - lowercase i, circumflex accent
                {"\u00EF", "&iuml;"}, // � - lowercase i, umlaut
                {"\u00F0", "&eth;"}, // � - lowercase eth, Icelandic
                {"\u00F1", "&ntilde;"}, // � - lowercase n, tilde
                {"\u00F2", "&ograve;"}, // � - lowercase o, grave accent
                {"\u00F3", "&oacute;"}, // � - lowercase o, acute accent
                {"\u00F4", "&ocirc;"}, // � - lowercase o, circumflex accent
                {"\u00F5", "&otilde;"}, // � - lowercase o, tilde
                {"\u00F6", "&ouml;"}, // � - lowercase o, umlaut
                {"\u00F7", "&divide;"}, // division sign
                {"\u00F8", "&oslash;"}, // � - lowercase o, slash
                {"\u00F9", "&ugrave;"}, // � - lowercase u, grave accent
                {"\u00FA", "&uacute;"}, // � - lowercase u, acute accent
                {"\u00FB", "&ucirc;"}, // � - lowercase u, circumflex accent
                {"\u00FC", "&uuml;"}, // � - lowercase u, umlaut
                {"\u00FD", "&yacute;"}, // � - lowercase y, acute accent
                {"\u00FE", "&thorn;"}, // � - lowercase thorn, Icelandic
                {"\u00FF", "&yuml;"}, // � - lowercase y, umlaut
        };

        /**
         * Reverse of {@link #ISO8859_1_ESCAPE()} for unescaping purposes.
         *
         * @return the mapping table
         */
        private static String[][] ISO8859_1_UNESCAPE() {
            return ISO8859_1_UNESCAPE.clone();
        }

        private static final String[][] ISO8859_1_UNESCAPE = invert(ISO8859_1_ESCAPE);

        /**
         * Mapping to escape additional <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">character entity
         * references</a>. Note that this must be used with {@link #ISO8859_1_ESCAPE()} to get the full list of
         * HTML 4.0 character entities.
         *
         * @return the mapping table
         */
        private static String[][] HTML40_EXTENDED_ESCAPE() {
            return HTML40_EXTENDED_ESCAPE.clone();
        }

        private static final String[][] HTML40_EXTENDED_ESCAPE = {
                // <!-- Latin Extended-B -->
                {"\u0192", "&fnof;"}, // latin small f with hook = function= florin, U+0192 ISOtech -->
                // <!-- Greek -->
                {"\u0391", "&Alpha;"}, // greek capital letter alpha, U+0391 -->
                {"\u0392", "&Beta;"}, // greek capital letter beta, U+0392 -->
                {"\u0393", "&Gamma;"}, // greek capital letter gamma,U+0393 ISOgrk3 -->
                {"\u0394", "&Delta;"}, // greek capital letter delta,U+0394 ISOgrk3 -->
                {"\u0395", "&Epsilon;"}, // greek capital letter epsilon, U+0395 -->
                {"\u0396", "&Zeta;"}, // greek capital letter zeta, U+0396 -->
                {"\u0397", "&Eta;"}, // greek capital letter eta, U+0397 -->
                {"\u0398", "&Theta;"}, // greek capital letter theta,U+0398 ISOgrk3 -->
                {"\u0399", "&Iota;"}, // greek capital letter iota, U+0399 -->
                {"\u039A", "&Kappa;"}, // greek capital letter kappa, U+039A -->
                {"\u039B", "&Lambda;"}, // greek capital letter lambda,U+039B ISOgrk3 -->
                {"\u039C", "&Mu;"}, // greek capital letter mu, U+039C -->
                {"\u039D", "&Nu;"}, // greek capital letter nu, U+039D -->
                {"\u039E", "&Xi;"}, // greek capital letter xi, U+039E ISOgrk3 -->
                {"\u039F", "&Omicron;"}, // greek capital letter omicron, U+039F -->
                {"\u03A0", "&Pi;"}, // greek capital letter pi, U+03A0 ISOgrk3 -->
                {"\u03A1", "&Rho;"}, // greek capital letter rho, U+03A1 -->
                // <!-- there is no Sigmaf, and no U+03A2 character either -->
                {"\u03A3", "&Sigma;"}, // greek capital letter sigma,U+03A3 ISOgrk3 -->
                {"\u03A4", "&Tau;"}, // greek capital letter tau, U+03A4 -->
                {"\u03A5", "&Upsilon;"}, // greek capital letter upsilon,U+03A5 ISOgrk3 -->
                {"\u03A6", "&Phi;"}, // greek capital letter phi,U+03A6 ISOgrk3 -->
                {"\u03A7", "&Chi;"}, // greek capital letter chi, U+03A7 -->
                {"\u03A8", "&Psi;"}, // greek capital letter psi,U+03A8 ISOgrk3 -->
                {"\u03A9", "&Omega;"}, // greek capital letter omega,U+03A9 ISOgrk3 -->
                {"\u03B1", "&alpha;"}, // greek small letter alpha,U+03B1 ISOgrk3 -->
                {"\u03B2", "&beta;"}, // greek small letter beta, U+03B2 ISOgrk3 -->
                {"\u03B3", "&gamma;"}, // greek small letter gamma,U+03B3 ISOgrk3 -->
                {"\u03B4", "&delta;"}, // greek small letter delta,U+03B4 ISOgrk3 -->
                {"\u03B5", "&epsilon;"}, // greek small letter epsilon,U+03B5 ISOgrk3 -->
                {"\u03B6", "&zeta;"}, // greek small letter zeta, U+03B6 ISOgrk3 -->
                {"\u03B7", "&eta;"}, // greek small letter eta, U+03B7 ISOgrk3 -->
                {"\u03B8", "&theta;"}, // greek small letter theta,U+03B8 ISOgrk3 -->
                {"\u03B9", "&iota;"}, // greek small letter iota, U+03B9 ISOgrk3 -->
                {"\u03BA", "&kappa;"}, // greek small letter kappa,U+03BA ISOgrk3 -->
                {"\u03BB", "&lambda;"}, // greek small letter lambda,U+03BB ISOgrk3 -->
                {"\u03BC", "&mu;"}, // greek small letter mu, U+03BC ISOgrk3 -->
                {"\u03BD", "&nu;"}, // greek small letter nu, U+03BD ISOgrk3 -->
                {"\u03BE", "&xi;"}, // greek small letter xi, U+03BE ISOgrk3 -->
                {"\u03BF", "&omicron;"}, // greek small letter omicron, U+03BF NEW -->
                {"\u03C0", "&pi;"}, // greek small letter pi, U+03C0 ISOgrk3 -->
                {"\u03C1", "&rho;"}, // greek small letter rho, U+03C1 ISOgrk3 -->
                {"\u03C2", "&sigmaf;"}, // greek small letter final sigma,U+03C2 ISOgrk3 -->
                {"\u03C3", "&sigma;"}, // greek small letter sigma,U+03C3 ISOgrk3 -->
                {"\u03C4", "&tau;"}, // greek small letter tau, U+03C4 ISOgrk3 -->
                {"\u03C5", "&upsilon;"}, // greek small letter upsilon,U+03C5 ISOgrk3 -->
                {"\u03C6", "&phi;"}, // greek small letter phi, U+03C6 ISOgrk3 -->
                {"\u03C7", "&chi;"}, // greek small letter chi, U+03C7 ISOgrk3 -->
                {"\u03C8", "&psi;"}, // greek small letter psi, U+03C8 ISOgrk3 -->
                {"\u03C9", "&omega;"}, // greek small letter omega,U+03C9 ISOgrk3 -->
                {"\u03D1", "&thetasym;"}, // greek small letter theta symbol,U+03D1 NEW -->
                {"\u03D2", "&upsih;"}, // greek upsilon with hook symbol,U+03D2 NEW -->
                {"\u03D6", "&piv;"}, // greek pi symbol, U+03D6 ISOgrk3 -->
                // <!-- General Punctuation -->
                {"\u2022", "&bull;"}, // bullet = black small circle,U+2022 ISOpub -->
                // <!-- bullet is NOT the same as bullet operator, U+2219 -->
                {"\u2026", "&hellip;"}, // horizontal ellipsis = three dot leader,U+2026 ISOpub -->
                {"\u2032", "&prime;"}, // prime = minutes = feet, U+2032 ISOtech -->
                {"\u2033", "&Prime;"}, // double prime = seconds = inches,U+2033 ISOtech -->
                {"\u203E", "&oline;"}, // overline = spacing overscore,U+203E NEW -->
                {"\u2044", "&frasl;"}, // fraction slash, U+2044 NEW -->
                // <!-- Letterlike Symbols -->
                {"\u2118", "&weierp;"}, // script capital P = power set= Weierstrass p, U+2118 ISOamso -->
                {"\u2111", "&image;"}, // blackletter capital I = imaginary part,U+2111 ISOamso -->
                {"\u211C", "&real;"}, // blackletter capital R = real part symbol,U+211C ISOamso -->
                {"\u2122", "&trade;"}, // trade mark sign, U+2122 ISOnum -->
                {"\u2135", "&alefsym;"}, // alef symbol = first transfinite cardinal,U+2135 NEW -->
                // <!-- alef symbol is NOT the same as hebrew letter alef,U+05D0 although the
                // same glyph could be used to depict both characters -->
                // <!-- Arrows -->
                {"\u2190", "&larr;"}, // leftwards arrow, U+2190 ISOnum -->
                {"\u2191", "&uarr;"}, // upwards arrow, U+2191 ISOnum-->
                {"\u2192", "&rarr;"}, // rightwards arrow, U+2192 ISOnum -->
                {"\u2193", "&darr;"}, // downwards arrow, U+2193 ISOnum -->
                {"\u2194", "&harr;"}, // left right arrow, U+2194 ISOamsa -->
                {"\u21B5", "&crarr;"}, // downwards arrow with corner leftwards= carriage return, U+21B5 NEW -->
                {"\u21D0", "&lArr;"}, // leftwards double arrow, U+21D0 ISOtech -->
                // <!-- ISO 10646 does not say that lArr is the same as the 'is implied by'
                // arrow but also does not have any other character for that function.
                // So ? lArr canbe used for 'is implied by' as ISOtech suggests -->
                {"\u21D1", "&uArr;"}, // upwards double arrow, U+21D1 ISOamsa -->
                {"\u21D2", "&rArr;"}, // rightwards double arrow,U+21D2 ISOtech -->
                // <!-- ISO 10646 does not say this is the 'implies' character but does not
                // have another character with this function so ?rArr can be used for
                // 'implies' as ISOtech suggests -->
                {"\u21D3", "&dArr;"}, // downwards double arrow, U+21D3 ISOamsa -->
                {"\u21D4", "&hArr;"}, // left right double arrow,U+21D4 ISOamsa -->
                // <!-- Mathematical Operators -->
                {"\u2200", "&forall;"}, // for all, U+2200 ISOtech -->
                {"\u2202", "&part;"}, // partial differential, U+2202 ISOtech -->
                {"\u2203", "&exist;"}, // there exists, U+2203 ISOtech -->
                {"\u2205", "&empty;"}, // empty set = null set = diameter,U+2205 ISOamso -->
                {"\u2207", "&nabla;"}, // nabla = backward difference,U+2207 ISOtech -->
                {"\u2208", "&isin;"}, // element of, U+2208 ISOtech -->
                {"\u2209", "&notin;"}, // not an element of, U+2209 ISOtech -->
                {"\u220B", "&ni;"}, // contains as member, U+220B ISOtech -->
                // <!-- should there be a more memorable name than 'ni'? -->
                {"\u220F", "&prod;"}, // n-ary product = product sign,U+220F ISOamsb -->
                // <!-- prod is NOT the same character as U+03A0 'greek capital letter pi'
                // though the same glyph might be used for both -->
                {"\u2211", "&sum;"}, // n-ary summation, U+2211 ISOamsb -->
                // <!-- sum is NOT the same character as U+03A3 'greek capital letter sigma'
                // though the same glyph might be used for both -->
                {"\u2212", "&minus;"}, // minus sign, U+2212 ISOtech -->
                {"\u2217", "&lowast;"}, // asterisk operator, U+2217 ISOtech -->
                {"\u221A", "&radic;"}, // square root = radical sign,U+221A ISOtech -->
                {"\u221D", "&prop;"}, // proportional to, U+221D ISOtech -->
                {"\u221E", "&infin;"}, // infinity, U+221E ISOtech -->
                {"\u2220", "&ang;"}, // angle, U+2220 ISOamso -->
                {"\u2227", "&and;"}, // logical and = wedge, U+2227 ISOtech -->
                {"\u2228", "&or;"}, // logical or = vee, U+2228 ISOtech -->
                {"\u2229", "&cap;"}, // intersection = cap, U+2229 ISOtech -->
                {"\u222A", "&cup;"}, // union = cup, U+222A ISOtech -->
                {"\u222B", "&int;"}, // integral, U+222B ISOtech -->
                {"\u2234", "&there4;"}, // therefore, U+2234 ISOtech -->
                {"\u223C", "&sim;"}, // tilde operator = varies with = similar to,U+223C ISOtech -->
                // <!-- tilde operator is NOT the same character as the tilde, U+007E,although
                // the same glyph might be used to represent both -->
                {"\u2245", "&cong;"}, // approximately equal to, U+2245 ISOtech -->
                {"\u2248", "&asymp;"}, // almost equal to = asymptotic to,U+2248 ISOamsr -->
                {"\u2260", "&ne;"}, // not equal to, U+2260 ISOtech -->
                {"\u2261", "&equiv;"}, // identical to, U+2261 ISOtech -->
                {"\u2264", "&le;"}, // less-than or equal to, U+2264 ISOtech -->
                {"\u2265", "&ge;"}, // greater-than or equal to,U+2265 ISOtech -->
                {"\u2282", "&sub;"}, // subset of, U+2282 ISOtech -->
                {"\u2283", "&sup;"}, // superset of, U+2283 ISOtech -->
                // <!-- note that nsup, 'not a superset of, U+2283' is not covered by the
                // Symbol font encoding and is not included. Should it be, for symmetry?
                // It is in ISOamsn --> <!ENTITY nsub", "8836"},
                // not a subset of, U+2284 ISOamsn -->
                {"\u2286", "&sube;"}, // subset of or equal to, U+2286 ISOtech -->
                {"\u2287", "&supe;"}, // superset of or equal to,U+2287 ISOtech -->
                {"\u2295", "&oplus;"}, // circled plus = direct sum,U+2295 ISOamsb -->
                {"\u2297", "&otimes;"}, // circled times = vector product,U+2297 ISOamsb -->
                {"\u22A5", "&perp;"}, // up tack = orthogonal to = perpendicular,U+22A5 ISOtech -->
                {"\u22C5", "&sdot;"}, // dot operator, U+22C5 ISOamsb -->
                // <!-- dot operator is NOT the same character as U+00B7 middle dot -->
                // <!-- Miscellaneous Technical -->
                {"\u2308", "&lceil;"}, // left ceiling = apl upstile,U+2308 ISOamsc -->
                {"\u2309", "&rceil;"}, // right ceiling, U+2309 ISOamsc -->
                {"\u230A", "&lfloor;"}, // left floor = apl downstile,U+230A ISOamsc -->
                {"\u230B", "&rfloor;"}, // right floor, U+230B ISOamsc -->
                {"\u2329", "&lang;"}, // left-pointing angle bracket = bra,U+2329 ISOtech -->
                // <!-- lang is NOT the same character as U+003C 'less than' or U+2039 'single left-pointing angle quotation
                // mark' -->
                {"\u232A", "&rang;"}, // right-pointing angle bracket = ket,U+232A ISOtech -->
                // <!-- rang is NOT the same character as U+003E 'greater than' or U+203A
                // 'single right-pointing angle quotation mark' -->
                // <!-- Geometric Shapes -->
                {"\u25CA", "&loz;"}, // lozenge, U+25CA ISOpub -->
                // <!-- Miscellaneous Symbols -->
                {"\u2660", "&spades;"}, // black spade suit, U+2660 ISOpub -->
                // <!-- black here seems to mean filled as opposed to hollow -->
                {"\u2663", "&clubs;"}, // black club suit = shamrock,U+2663 ISOpub -->
                {"\u2665", "&hearts;"}, // black heart suit = valentine,U+2665 ISOpub -->
                {"\u2666", "&diams;"}, // black diamond suit, U+2666 ISOpub -->

                // <!-- Latin Extended-A -->
                {"\u0152", "&OElig;"}, // -- latin capital ligature OE,U+0152 ISOlat2 -->
                {"\u0153", "&oelig;"}, // -- latin small ligature oe, U+0153 ISOlat2 -->
                // <!-- ligature is a misnomer, this is a separate character in some languages -->
                {"\u0160", "&Scaron;"}, // -- latin capital letter S with caron,U+0160 ISOlat2 -->
                {"\u0161", "&scaron;"}, // -- latin small letter s with caron,U+0161 ISOlat2 -->
                {"\u0178", "&Yuml;"}, // -- latin capital letter Y with diaeresis,U+0178 ISOlat2 -->
                // <!-- Spacing Modifier Letters -->
                {"\u02C6", "&circ;"}, // -- modifier letter circumflex accent,U+02C6 ISOpub -->
                {"\u02DC", "&tilde;"}, // small tilde, U+02DC ISOdia -->
                // <!-- General Punctuation -->
                {"\u2002", "&ensp;"}, // en space, U+2002 ISOpub -->
                {"\u2003", "&emsp;"}, // em space, U+2003 ISOpub -->
                {"\u2009", "&thinsp;"}, // thin space, U+2009 ISOpub -->
                {"\u200C", "&zwnj;"}, // zero width non-joiner,U+200C NEW RFC 2070 -->
                {"\u200D", "&zwj;"}, // zero width joiner, U+200D NEW RFC 2070 -->
                {"\u200E", "&lrm;"}, // left-to-right mark, U+200E NEW RFC 2070 -->
                {"\u200F", "&rlm;"}, // right-to-left mark, U+200F NEW RFC 2070 -->
                {"\u2013", "&ndash;"}, // en dash, U+2013 ISOpub -->
                {"\u2014", "&mdash;"}, // em dash, U+2014 ISOpub -->
                {"\u2018", "&lsquo;"}, // left single quotation mark,U+2018 ISOnum -->
                {"\u2019", "&rsquo;"}, // right single quotation mark,U+2019 ISOnum -->
                {"\u201A", "&sbquo;"}, // single low-9 quotation mark, U+201A NEW -->
                {"\u201C", "&ldquo;"}, // left double quotation mark,U+201C ISOnum -->
                {"\u201D", "&rdquo;"}, // right double quotation mark,U+201D ISOnum -->
                {"\u201E", "&bdquo;"}, // double low-9 quotation mark, U+201E NEW -->
                {"\u2020", "&dagger;"}, // dagger, U+2020 ISOpub -->
                {"\u2021", "&Dagger;"}, // double dagger, U+2021 ISOpub -->
                {"\u2030", "&permil;"}, // per mille sign, U+2030 ISOtech -->
                {"\u2039", "&lsaquo;"}, // single left-pointing angle quotation mark,U+2039 ISO proposed -->
                // <!-- lsaquo is proposed but not yet ISO standardized -->
                {"\u203A", "&rsaquo;"}, // single right-pointing angle quotation mark,U+203A ISO proposed -->
                // <!-- rsaquo is proposed but not yet ISO standardized -->
                {"\u20AC", "&euro;"}, // -- euro sign, U+20AC NEW -->
        };

        /**
         * Reverse of {@link #HTML40_EXTENDED_ESCAPE()} for unescaping purposes.
         *
         * @return the mapping table
         */
        private static String[][] HTML40_EXTENDED_UNESCAPE() {
            return HTML40_EXTENDED_UNESCAPE.clone();
        }

        private static final String[][] HTML40_EXTENDED_UNESCAPE = invert(HTML40_EXTENDED_ESCAPE);

        /**
         * Mapping to escape the basic XML and HTML character entities.
         * <p>
         * Namely: {@code " & < >}
         *
         * @return the mapping table
         */
        private static String[][] BASIC_ESCAPE() {
            return BASIC_ESCAPE.clone();
        }

        private static final String[][] BASIC_ESCAPE = {
                {"\"", "&quot;"}, // " - double-quote
                {"&", "&amp;"},   // & - ampersand
                {"<", "&lt;"},    // < - less-than
                {">", "&gt;"},    // > - greater-than
        };

        /**
         * Reverse of {@link #BASIC_ESCAPE()} for unescaping purposes.
         *
         * @return the mapping table
         */
        private static String[][] BASIC_UNESCAPE() {
            return BASIC_UNESCAPE.clone();
        }

        private static final String[][] BASIC_UNESCAPE = invert(BASIC_ESCAPE);

        /**
         * Mapping to escape the apostrophe character to its XML character entity.
         *
         * @return the mapping table
         */
        private static String[][] APOS_ESCAPE() {
            return APOS_ESCAPE.clone();
        }

        private static final String[][] APOS_ESCAPE = {
                {"'", "&apos;"}, // XML apostrophe
        };

        /**
         * Reverse of {@link #APOS_ESCAPE()} for unescaping purposes.
         *
         * @return the mapping table
         */
        private static String[][] APOS_UNESCAPE() {
            return APOS_UNESCAPE.clone();
        }

        private static final String[][] APOS_UNESCAPE = invert(APOS_ESCAPE);

        /**
         * Mapping to escape the Java control characters.
         * <p>
         * Namely: {@code \b \n \t \f \r}
         *
         * @return the mapping table
         */
        private static String[][] JAVA_CTRL_CHARS_ESCAPE() {
            return JAVA_CTRL_CHARS_ESCAPE.clone();
        }

        private static final String[][] JAVA_CTRL_CHARS_ESCAPE = {
                {"\b", "\\b"},
                {"\n", "\\n"},
                {"\t", "\\t"},
                {"\f", "\\f"},
                {"\r", "\\r"}
        };

        /**
         * Reverse of {@link #JAVA_CTRL_CHARS_ESCAPE()} for unescaping purposes.
         *
         * @return the mapping table
         */
        private static String[][] JAVA_CTRL_CHARS_UNESCAPE() {
            return JAVA_CTRL_CHARS_UNESCAPE.clone();
        }

        private static final String[][] JAVA_CTRL_CHARS_UNESCAPE = invert(JAVA_CTRL_CHARS_ESCAPE);

        /**
         * Used to invert an escape array into an unescape array
         *
         * @param array String[][] to be inverted
         * @return String[][] inverted array
         */
        private static String[][] invert(final String[][] array) {
            final String[][] newarray = new String[array.length][2];
            for (int i = 0; i < array.length; i++) {
                newarray[i][0] = array[i][1];
                newarray[i][1] = array[i][0];
            }
            return newarray;
        }

    }

    /**
     * Translates a value using a lookup table.
     *
     * @version $Id: LookupTranslator.java 1669520 2015-03-27 08:03:41Z britter $
     * @since 3.0
     */
    private static class LookupTranslator extends CharSequenceTranslator {

        private final HashMap<String, String> lookupMap;
        private final HashSet<Character> prefixSet;
        private final int shortest;
        private final int longest;

        /**
         * Define the lookup table to be used in translation
         * <p>
         * Note that, as of Lang 3.1, the key to the lookup table is converted to a
         * java.lang.String. This is because we need the key to support hashCode and
         * equals(Object), allowing it to be the key for a HashMap. See LANG-882.
         *
         * @param lookup CharSequence[][] table of size [*][2]
         */
        public LookupTranslator(final CharSequence[]... lookup) {
            lookupMap = new HashMap<>();
            prefixSet = new HashSet<>();
            int _shortest = Integer.MAX_VALUE;
            int _longest = 0;
            if (lookup != null) {
                for (final CharSequence[] seq : lookup) {
                    this.lookupMap.put(seq[0].toString(), seq[1].toString());
                    this.prefixSet.add(seq[0].charAt(0));
                    final int sz = seq[0].length();
                    if (sz < _shortest) {
                        _shortest = sz;
                    }
                    if (sz > _longest) {
                        _longest = sz;
                    }
                }
            }
            shortest = _shortest;
            longest = _longest;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
            // check if translation exists for the input at position index
            if (prefixSet.contains(input.charAt(index))) {
                int max = longest;
                if (index + longest > input.length()) {
                    max = input.length() - index;
                }
                // implement greedy algorithm by trying maximum match first
                for (int i = max; i >= shortest; i--) {
                    final CharSequence subSeq = input.subSequence(index, index + i);
                    final String result = lookupMap.get(subSeq.toString());

                    if (result != null) {
                        out.write(result);
                        return i;
                    }
                }
            }
            return 0;
        }
    }

    /**
     * Translate escaped octal Strings back to their octal values.
     * <p>
     * For example, "\45" should go back to being the specific value (a %).
     * <p>
     * Note that this currently only supports the viable range of octal for Java; namely
     * 1 to 377. This is because parsing Java is the main use case.
     *
     * @version $Id: OctalUnescaper.java 967237 2010-07-23 20:08:57Z mbenson $
     * @since 3.0
     */
    private static class OctalUnescaper extends CharSequenceTranslator {

        /**
         * {@inheritDoc}
         */
        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
            final int remaining = input.length() - index - 1; // how many characters left, ignoring the first \
            final StringBuilder builder = new StringBuilder();
            if (input.charAt(index) == '\\' && remaining > 0 && isOctalDigit(input.charAt(index + 1))) {
                final int next = index + 1;
                final int next2 = index + 2;
                final int next3 = index + 3;

                // we know this is good as we checked it in the if block above
                builder.append(input.charAt(next));

                if (remaining > 1 && isOctalDigit(input.charAt(next2))) {
                    builder.append(input.charAt(next2));
                    if (remaining > 2 && isZeroToThree(input.charAt(next)) && isOctalDigit(input.charAt(next3))) {
                        builder.append(input.charAt(next3));
                    }
                }

                out.write(Integer.parseInt(builder.toString(), 8));
                return 1 + builder.length();
            }
            return 0;
        }

        /**
         * Checks if the given char is an octal digit. Octal digits are the character representations of the digits 0 to 7.
         *
         * @param ch the char to check
         * @return true if the given char is the character representation of one of the digits from 0 to 7
         */
        private boolean isOctalDigit(final char ch) {
            return ch >= '0' && ch <= '7';
        }

        /**
         * Checks if the given char is the character representation of one of the digit from 0 to 3.
         *
         * @param ch the char to check
         * @return true if the given char is the character representation of one of the digits from 0 to 3
         */
        private boolean isZeroToThree(final char ch) {
            return ch >= '0' && ch <= '3';
        }
    }

    /**
     * Translates escaped Unicode values of the form \\u+\d\d\d\d back to
     * Unicode. It supports multiple 'u' characters and will work with or
     * without the +.
     *
     * @version $Id: UnicodeUnescaper.java 1606060 2014-06-27 12:33:07Z ggregory $
     * @since 3.0
     */
    private static class UnicodeUnescaper extends CharSequenceTranslator {

        /**
         * {@inheritDoc}
         */
        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
            if (input.charAt(index) == '\\' && index + 1 < input.length() && input.charAt(index + 1) == 'u') {
                // consume optional additional 'u' chars
                int i = 2;
                while (index + i < input.length() && input.charAt(index + i) == 'u') {
                    i++;
                }

                if (index + i < input.length() && input.charAt(index + i) == '+') {
                    i++;
                }

                if (index + i + 4 <= input.length()) {
                    // Get 4 hex digits
                    final CharSequence unicode = input.subSequence(index + i, index + i + 4);

                    try {
                        final int value = Integer.parseInt(unicode.toString(), 16);
                        out.write((char) value);
                    } catch (final NumberFormatException nfe) {
                        throw new IllegalArgumentException("Unable to parse unicode value: " + unicode, nfe);
                    }
                    return i + 4;
                }
                throw new IllegalArgumentException("Less than 4 hex digits in unicode value: '" + input.subSequence(index, input.length())
                        + "' due to end of CharSequence");
            }
            return 0;
        }
    }
}

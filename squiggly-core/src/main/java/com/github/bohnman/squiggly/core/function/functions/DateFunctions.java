package com.github.bohnman.squiggly.core.function.functions;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;
import java.util.Date;

/**
 * Date and time functions.
 */
public class DateFunctions {

    private DateFunctions() {
    }

    /**
     * Increment a date.
     *
     * @param dateTime the date
     * @param number   number to add
     * @param unit     add units
     * @return added datetime
     * @see TemporalUnit
     */
    public static LocalDateTime add(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    /**
     * Increment a date.
     *
     * @param dateTime the date
     * @param number   number to add
     * @param unit     add units
     * @return added datetime
     * @see TemporalUnit
     */
    public static ZonedDateTime add(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    /**
     * Ceiling a date to the nearest number units.
     *
     * @param dateTime the date
     * @param number   number to ceil by
     * @param unit     ceil units
     * @return ceil datetime
     * @see TemporalUnit
     */
    public static LocalDateTime ceil(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit).plus(1, temporalUnit);
    }

    /**
     * Ceiling a date to the nearest number units.
     *
     * @param dateTime the date
     * @param number   number to ceil by
     * @param unit     ceil units
     * @return ceil datetime
     * @see TemporalUnit
     */
    public static ZonedDateTime ceil(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit).plus(1, temporalUnit);
    }

    /**
     * Floor a date to the nearest number units.
     *
     * @param dateTime the date
     * @param number   number to floor by
     * @param unit     floor units
     * @return floor datetime
     * @see TemporalUnit
     */
    public static LocalDateTime floor(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit);
    }

    /**
     * Floor a date to the nearest number units.
     *
     * @param dateTime the date
     * @param number   number to floor by
     * @param unit     floor units
     * @return floor datetime
     * @see TemporalUnit
     */
    public static ZonedDateTime floor(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        TemporalUnit temporalUnit = toTemporalUnit(unit);
        return dateTime.truncatedTo(temporalUnit);
    }

    /**
     * Format a date in ISO-8601 format.
     *
     * @param dateTime date
     * @return formatted date
     */
    public static String format(LocalDateTime dateTime) {
        return format(dateTime, "yyyy-MM-dd'T'HH:mm:ss.SSS");
    }

    /**
     * Format a date according to the given pattern.
     *
     * @param dateTime date
     * @return formatted date
     * @see DateTimeFormatter
     */
    public static String format(LocalDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    /**
     * Format a date in ISO-8601 format.
     *
     * @param dateTime date
     * @return formatted date
     */
    public static String format(OffsetDateTime dateTime) {
        return format(dateTime.toZonedDateTime(), "yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    }

    /**
     * Format a date according to the given pattern.
     *
     * @param dateTime date
     * @return formatted date
     * @see DateTimeFormatter
     */
    public static String format(ZonedDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    /**
     * Retrieve the current date that contains time zone information.
     *
     * @return current date
     */
    // Date functions
    public static ZonedDateTime now() {
        return ZonedDateTime.now();
    }

    /**
     * Retrieve the current date that contains no time zone information.
     *
     * @return current date
     */
    public static LocalDateTime nowLocal() {
        return LocalDateTime.now();
    }

    /**
     * Parse a string into a date with time zone, using default patterns.
     * <p>
     * Default patterns are:
     * <ul>
     * <li>yyyy-MM-dd'T'HH:mm:ss.SSSZ</li>
     * <li>yyyy-MM-dd'T'HH:mm:ssZ</li>
     * <li>yyyy-MM-dd'T'HH:mmZ</li>
     * <li>yyyy-MM-dd'T'HH:mm:ss.SSS</li>
     * <li>yyyy-MM-dd</li>
     * <li>yyyy-MM-dd'T'HH:mm:ss.SSS"</li>
     * <li>yyyy-MM-dd'T'HH:mm:ss</li>
     * <li>yyyy-MM-dd'T'HH:mm</li>
     * </ul>
     *
     * @param input formatted date
     * @return date
     */
    public static ZonedDateTime parseDate(String input) {
        return parseDate(input,
                "yyyy-MM-dd'T'HH:mm:ss.SSSZ",
                "yyyy-MM-dd'T'HH:mm:ssZ",
                "yyyy-MM-dd'T'HH:mmZ",
                "yyyy-MM-dd'T'HH:mm:ss.SSS",
                "yyyy-MM-dd",
                "yyyy-MM-dd'T'HH:mm:ss.SSS",
                "yyyy-MM-dd'T'HH:mm:ss",
                "yyyy-MM-dd'T'HH:mm");
    }

    /**
     * Parse the date with time zone using the given patterns, in order.  The reason this method has a main pattern
     * and other patterns is to distinguish it between parseDate with no patterns.
     *
     * @param input         formatted date
     * @param mainPattern   pattern 1
     * @param otherPatterns other patterns
     * @return date
     */
    public static ZonedDateTime parseDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault());
    }

    /**
     * Parse a string into a date with no time zone, using default patterns.
     * <p>
     * Default patterns are:
     * <ul>
     * <li>yyyy-MM-dd'T'HH:mm:ss.SSS</li>
     * <li>yyyy-MM-dd</li>
     * <li>yyyyy-MM-dd'T'HH:mm:ss.SSS</li>
     * <li>yyyy-MM-dd'T'HH:mm:ss</li>
     * <li>yyyy-MM-dd'T'HH:mm</li>
     * </ul>
     *
     * @param input formatted date
     * @return date
     */
    public static LocalDateTime parseLocalDate(String input) {
        return parseLocalDate(input,
                "yyyy-MM-dd'T'HH:mm:ss.SSS",
                "yyyy-MM-dd",
                "yyyy-MM-dd'T'HH:mm:ss.SSS",
                "yyyy-MM-dd'T'HH:mm:ss",
                "yyyy-MM-dd'T'HH:mm");
    }

    /**
     * Parse the date with no time zone using the given patterns, in order.  The reason this method has a main pattern
     * and other patterns is to distinguish it between parseDate with no patterns.
     *
     * @param input         formatted date
     * @param mainPattern   pattern 1
     * @param otherPatterns other patterns
     * @return date
     */

    public static LocalDateTime parseLocalDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    /**
     * Decrement a date.
     *
     * @param dateTime the date
     * @param number   number to subtract
     * @param unit     subtract units
     * @return subtacted datetime
     * @see TemporalUnit
     */
    public static LocalDateTime subtract(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    /**
     * Decrement a date.
     *
     * @param dateTime the date
     * @param number   number to subtract
     * @param unit     subtract units
     * @return subtacted datetime
     * @see TemporalUnit
     */
    public static ZonedDateTime subtract(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    private static Date parseDateInternal(String input, String mainPattern, String... otherPatterns) {

        try {
            return new SimpleDateFormat(mainPattern).parse(input);
        } catch (ParseException e) {
            // ignore;
        }

        for (String otherPattern : otherPatterns) {
            try {
                return new SimpleDateFormat(otherPattern).parse(input);
            } catch (ParseException e) {
                // ignore
            }
        }

        throw new IllegalArgumentException("Unable to parse input");
    }

    private static TemporalUnit toTemporalUnit(String unit) {
        if (unit == null) {
            return ChronoUnit.MILLIS;
        }

        switch (unit) {
            case "nanos":
            case "nanosecond":
            case "nanoseconds":
            case "ns":
                return ChronoUnit.NANOS;
            case "millisecond":
            case "milliseconds":
            case "millis":
            case "ms":
                return ChronoUnit.MILLIS;
            case "s":
            case "sec":
            case "second":
            case "seconds":
                return ChronoUnit.SECONDS;
            case "min":
            case "minute":
            case "minutes":
            case "m":
                return ChronoUnit.MINUTES;
            case "hr":
            case "hrs":
            case "hour":
            case "hours":
            case "h":
                return ChronoUnit.HOURS;
            case "d":
            case "day":
            case "days":
                return ChronoUnit.DAYS;
            case "M":
            case "mon":
            case "month":
            case "months":
                return ChronoUnit.MONTHS;
            case "y":
            case "yr":
            case "yrs":
            case "year":
            case "years":
                return ChronoUnit.YEARS;
            default:
                throw new IllegalArgumentException("unrecognized temporal unit");
        }
    }


}

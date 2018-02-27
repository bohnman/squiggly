package com.github.bohnman.squiggly.core.function.functions;

import com.github.bohnman.squiggly.core.function.annotation.SquigglyMethod;

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

public class DateFunctions {

    private DateFunctions() {
    }


    // Date functions
    @SquigglyMethod
    public static ZonedDateTime now() {
        return ZonedDateTime.now();
    }

    @SquigglyMethod
    public static LocalDateTime nowLocal() {
        return LocalDateTime.now();
    }


    @SquigglyMethod
    public static String format(LocalDateTime dateTime) {
        return format(dateTime, "yyyy-MM-dd'T'HH:mm:ss.SSS");
    }

    @SquigglyMethod
    public static String format(LocalDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    @SquigglyMethod
    public static String format(OffsetDateTime dateTime) {
        return format(dateTime.toZonedDateTime(), "yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    }

    @SquigglyMethod
    public static String format(ZonedDateTime dateTime, String pattern) {
        if (dateTime == null) {
            return "";
        }

        return dateTime.format(DateTimeFormatter.ofPattern(pattern));
    }

    @SquigglyMethod
    public static LocalDateTime add(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static ZonedDateTime add(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.plus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static LocalDateTime subtract(LocalDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
    public static ZonedDateTime subtract(ZonedDateTime dateTime, Number number, String unit) {
        if (dateTime == null) {
            return null;
        }

        if (number == null) {
            return dateTime;
        }

        return dateTime.minus(number.intValue(), toTemporalUnit(unit));
    }

    @SquigglyMethod
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

    @SquigglyMethod
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

    @SquigglyMethod
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

    @SquigglyMethod
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

    @SquigglyMethod
    public static LocalDateTime parseLocalDate(String input) {
        return parseLocalDate(input, "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss", "yyyy-MM-dd'T'HH:mm");
    }

    @SquigglyMethod
    public static LocalDateTime parseLocalDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
    }

    @SquigglyMethod
    public static ZonedDateTime parseDate(String input) {
        return parseDate(input, "yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ssZ", "yyyy-MM-dd'T'HH:mmZ", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd", "yyyy-MM-dd'T'HH:mm:ss.SSS", "yyyy-MM-dd'T'HH:mm:ss", "yyyy-MM-dd'T'HH:mm");
    }

    @SquigglyMethod
    public static ZonedDateTime parseDate(String input, String mainPattern, String... otherPatterns) {
        return parseDateInternal(input, mainPattern, otherPatterns).toInstant().atZone(ZoneId.systemDefault());
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

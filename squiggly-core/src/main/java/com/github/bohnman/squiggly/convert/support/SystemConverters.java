package com.github.bohnman.squiggly.convert.support;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.library.CoreLibraries;
import com.github.bohnman.squiggly.convert.ConverterRecord;
import com.github.bohnman.squiggly.convert.SquigglyConverterRegistry;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.*;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Registers the built-in converters between base JVM types.
 */
public class SystemConverters {

    /**
     * Register the converters.
     *
     * @param registry the registry
     */
    public static void add(SquigglyConverterRegistry registry) {

        // Byte
        registry.add(Byte.class, Integer.class, Byte::intValue);
        registry.add(Byte.class, Short.class, Byte::shortValue);
        registry.add(Byte.class, Long.class, Byte::longValue);
        registry.add(Byte.class, Float.class, Byte::floatValue);
        registry.add(Byte.class, Double.class, Byte::doubleValue);
        registry.add(Byte.class, BigInteger.class, (Function<Byte, BigInteger>) BigInteger::valueOf);
        registry.add(Byte.class, BigDecimal.class, BigDecimal::valueOf);

        // Integer
        registry.add(Integer.class, Byte.class, Integer::byteValue);
        registry.add(Integer.class, Short.class, Integer::shortValue);
        registry.add(Integer.class, Long.class, Integer::longValue);
        registry.add(Integer.class, Float.class, Integer::floatValue);
        registry.add(Integer.class, Double.class, Integer::doubleValue);
        registry.add(Integer.class, BigInteger.class, (Function<Integer, BigInteger>) BigInteger::valueOf);
        registry.add(Integer.class, BigDecimal.class, BigDecimal::valueOf);

        // Short
        registry.add(Short.class, Byte.class, Short::byteValue);
        registry.add(Short.class, Integer.class, Short::intValue);
        registry.add(Short.class, Long.class, Short::longValue);
        registry.add(Short.class, Float.class, Short::floatValue);
        registry.add(Short.class, Double.class, Short::doubleValue);
        registry.add(Short.class, BigInteger.class, (Function<Short, BigInteger>) BigInteger::valueOf);
        registry.add(Short.class, BigDecimal.class, BigDecimal::valueOf);

        // Long
        registry.add(Long.class, Byte.class, Long::byteValue);
        registry.add(Long.class, Integer.class, Long::intValue);
        registry.add(Long.class, Short.class, Long::shortValue);
        registry.add(Long.class, Float.class, Long::floatValue);
        registry.add(Long.class, Double.class, Long::doubleValue);
        registry.add(Long.class, BigInteger.class, BigInteger::valueOf);
        registry.add(Long.class, BigDecimal.class, BigDecimal::valueOf);

        // Float
        registry.add(Float.class, Byte.class, Float::byteValue);
        registry.add(Float.class, Integer.class, Float::intValue);
        registry.add(Float.class, Long.class, Float::longValue);
        registry.add(Float.class, Short.class, Float::shortValue);
        registry.add(Float.class, Double.class, Float::doubleValue);
        registry.add(Float.class, BigInteger.class, f -> BigInteger.valueOf(f.longValue()));
        registry.add(Float.class, BigDecimal.class, BigDecimal::valueOf);

        // Double
        registry.add(Double.class, Byte.class, Double::byteValue);
        registry.add(Double.class, Integer.class, Double::intValue);
        registry.add(Double.class, Long.class, Double::longValue);
        registry.add(Double.class, Float.class, Double::floatValue);
        registry.add(Double.class, Short.class, Double::shortValue);
        registry.add(Double.class, BigInteger.class, d -> BigInteger.valueOf(d.longValue()));
        registry.add(Double.class, BigDecimal.class, BigDecimal::valueOf);

        // Big Integer
        registry.add(BigInteger.class, Byte.class, Number::byteValue);
        registry.add(BigInteger.class, Integer.class, Number::intValue);
        registry.add(BigInteger.class, Long.class, Number::longValue);
        registry.add(BigInteger.class, Float.class, Number::floatValue);
        registry.add(BigInteger.class, Short.class, Number::shortValue);
        registry.add(BigInteger.class, Double.class, Number::doubleValue);
        registry.add(BigInteger.class, BigDecimal.class, BigDecimal::new);

        // Big Decimal
        registry.add(BigDecimal.class, Byte.class, Number::byteValue);
        registry.add(BigDecimal.class, Integer.class, Number::intValue);
        registry.add(BigDecimal.class, Long.class, Number::longValue);
        registry.add(BigDecimal.class, Float.class, Number::floatValue);
        registry.add(BigDecimal.class, Short.class, Number::shortValue);
        registry.add(BigDecimal.class, Double.class, Number::doubleValue);
        registry.add(BigDecimal.class, BigInteger.class, BigDecimal::toBigInteger);

        // String
        registry.add(String.class, Boolean.class, Boolean::valueOf);
        registry.add(String.class, Byte.class, Byte::parseByte);
        registry.add(String.class, Integer.class, Integer::parseInt);
        registry.add(String.class, Long.class, Long::parseLong);
        registry.add(String.class, Short.class, Short::parseShort);
        registry.add(String.class, Float.class, Float::parseFloat);
        registry.add(String.class, Double.class, Double::parseDouble);
        registry.add(String.class, BigInteger.class, BigInteger::new);
        registry.add(String.class, BigDecimal.class, BigDecimal::new);

        // TimeZone
        registry.add(String.class, TimeZone.class, CoreConversions::toTimeZone);
        registry.add(ZoneId.class, TimeZone.class, TimeZone::getTimeZone);

        // Long to Date
        registry.add(Long.class, OffsetDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(Long.class, ZonedDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()));
        registry.add(Long.class, Date.class, Date::new);
        registry.add(Long.class, Instant.class, Instant::ofEpochMilli);
        registry.add(Long.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s);
            return c;
        });
        registry.add(Long.class, LocalDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.add(Long.class, LocalDate.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDate());
        registry.add(Long.class, LocalTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalTime());

        // Instant
        registry.add(Instant.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(Instant.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        registry.add(Instant.class, Date.class, Date::from);
        registry.add(Instant.class, Long.class, Instant::toEpochMilli);
        registry.add(Instant.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s.toEpochMilli());
            return c;
        });
        registry.add(Instant.class, LocalDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.add(Instant.class, LocalDate.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDate());
        registry.add(Instant.class, LocalTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalTime());

        // LocalDateTime
        registry.add(LocalDateTime.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(LocalDateTime.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        registry.add(LocalDateTime.class, Instant.class, s -> s.atZone(ZoneId.systemDefault()).toInstant());
        registry.add(LocalDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.atZone(ZoneId.systemDefault())));
        registry.add(LocalDateTime.class, Date.class, s -> Date.from(s.atZone(ZoneId.systemDefault()).toInstant()));
        registry.add(LocalDateTime.class, Long.class, s -> s.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
        registry.add(LocalDateTime.class, LocalDate.class, LocalDateTime::toLocalDate);
        registry.add(LocalDateTime.class, LocalTime.class, LocalDateTime::toLocalTime);

        // ZoneDateTime
        registry.add(ZonedDateTime.class, OffsetDateTime.class, ZonedDateTime::toOffsetDateTime);
        registry.add(ZonedDateTime.class, Instant.class, ZonedDateTime::toInstant);
        registry.add(ZonedDateTime.class, Calendar.class, GregorianCalendar::from);
        registry.add(ZonedDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        registry.add(ZonedDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        registry.add(ZonedDateTime.class, LocalDateTime.class, ZonedDateTime::toLocalDateTime);
        registry.add(ZonedDateTime.class, LocalDate.class, ZonedDateTime::toLocalDate);
        registry.add(ZonedDateTime.class, LocalTime.class, ZonedDateTime::toLocalTime);

        // OffsetDateTime
        registry.add(OffsetDateTime.class, ZonedDateTime.class, OffsetDateTime::toZonedDateTime);
        registry.add(OffsetDateTime.class, Instant.class, OffsetDateTime::toInstant);
        registry.add(OffsetDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.toZonedDateTime()));
        registry.add(OffsetDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        registry.add(OffsetDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        registry.add(OffsetDateTime.class, LocalDateTime.class, OffsetDateTime::toLocalDateTime);
        registry.add(OffsetDateTime.class, LocalDate.class, OffsetDateTime::toLocalDate);
        registry.add(OffsetDateTime.class, LocalTime.class, OffsetDateTime::toLocalTime);

        // Calendar
        registry.add(Calendar.class, Instant.class, Calendar::toInstant);
        registry.add(Calendar.class, Date.class, Calendar::getTime);
        registry.add(Calendar.class, Long.class, Calendar::getTimeInMillis);
        registry.add(Calendar.class, ZonedDateTime.class, SystemConverters::calendarToZonedDateTime);
        registry.add(Calendar.class, OffsetDateTime.class, s -> calendarToZonedDateTime(s).toOffsetDateTime());
        registry.add(Calendar.class, LocalDateTime.class, s -> calendarToZonedDateTime(s).toLocalDateTime());
        registry.add(Calendar.class, LocalDate.class, s -> calendarToZonedDateTime(s).toLocalDate());
        registry.add(Calendar.class, LocalTime.class, s -> calendarToZonedDateTime(s).toLocalTime());

        // Date
        registry.add(Date.class, ZonedDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()));
        registry.add(Date.class, Instant.class, Date::toInstant);
        registry.add(Date.class, OffsetDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(Date.class, Long.class, Date::getTime);
        registry.add(Date.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTime(s);
            return c;
        });
        registry.add(Date.class, LocalDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.add(Date.class, LocalDate.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDate());
        registry.add(Date.class, LocalTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalTime());

        if (CoreLibraries.isJodaTimePresent()) {
            JodaConverters.add(registry);
        }

        // Object
        registry.add(Object.class, String.class, CoreConversions::safeToString, ConverterRecord.MAX_ORDER);
        registry.add(Object.class, Function.class, CoreConversions::toFunction, ConverterRecord.MAX_ORDER);
        registry.add(Object.class, Predicate.class, CoreConversions::toPredicate, ConverterRecord.MAX_ORDER);
        registry.add(Object.class, CoreLambda.class, CoreConversions::toLambda, ConverterRecord.MAX_ORDER);

    }

    private static ZonedDateTime calendarToZonedDateTime(Calendar source) {
        if (source instanceof GregorianCalendar) {
            return ((GregorianCalendar) source).toZonedDateTime();
        } else {
            return ZonedDateTime.ofInstant(Instant.ofEpochMilli(source.getTimeInMillis()),
                    source.getTimeZone().toZoneId());
        }
    }
}
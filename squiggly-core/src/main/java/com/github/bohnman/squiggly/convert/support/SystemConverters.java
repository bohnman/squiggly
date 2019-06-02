package com.github.bohnman.squiggly.convert.support;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.squiggly.convert.ConverterDescriptor;
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
        registry.addConverter(Byte.class, Integer.class, Byte::intValue);
        registry.addConverter(Byte.class, Short.class, Byte::shortValue);
        registry.addConverter(Byte.class, Long.class, Byte::longValue);
        registry.addConverter(Byte.class, Float.class, Byte::floatValue);
        registry.addConverter(Byte.class, Double.class, Byte::doubleValue);
        registry.addConverter(Byte.class, BigInteger.class, (Function<Byte, BigInteger>) BigInteger::valueOf);
        registry.addConverter(Byte.class, BigDecimal.class, BigDecimal::valueOf);

        // Integer
        registry.addConverter(Integer.class, Byte.class, Integer::byteValue);
        registry.addConverter(Integer.class, Short.class, Integer::shortValue);
        registry.addConverter(Integer.class, Long.class, Integer::longValue);
        registry.addConverter(Integer.class, Float.class, Integer::floatValue);
        registry.addConverter(Integer.class, Double.class, Integer::doubleValue);
        registry.addConverter(Integer.class, BigInteger.class, (Function<Integer, BigInteger>) BigInteger::valueOf);
        registry.addConverter(Integer.class, BigDecimal.class, BigDecimal::valueOf);

        // Short
        registry.addConverter(Short.class, Byte.class, Short::byteValue);
        registry.addConverter(Short.class, Integer.class, Short::intValue);
        registry.addConverter(Short.class, Long.class, Short::longValue);
        registry.addConverter(Short.class, Float.class, Short::floatValue);
        registry.addConverter(Short.class, Double.class, Short::doubleValue);
        registry.addConverter(Short.class, BigInteger.class, (Function<Short, BigInteger>) BigInteger::valueOf);
        registry.addConverter(Short.class, BigDecimal.class, BigDecimal::valueOf);

        // Long
        registry.addConverter(Long.class, Byte.class, Long::byteValue);
        registry.addConverter(Long.class, Integer.class, Long::intValue);
        registry.addConverter(Long.class, Short.class, Long::shortValue);
        registry.addConverter(Long.class, Float.class, Long::floatValue);
        registry.addConverter(Long.class, Double.class, Long::doubleValue);
        registry.addConverter(Long.class, BigInteger.class, BigInteger::valueOf);
        registry.addConverter(Long.class, BigDecimal.class, BigDecimal::valueOf);

        // Float
        registry.addConverter(Float.class, Byte.class, Float::byteValue);
        registry.addConverter(Float.class, Integer.class, Float::intValue);
        registry.addConverter(Float.class, Long.class, Float::longValue);
        registry.addConverter(Float.class, Short.class, Float::shortValue);
        registry.addConverter(Float.class, Double.class, Float::doubleValue);
        registry.addConverter(Float.class, BigInteger.class, f -> BigInteger.valueOf(f.longValue()));
        registry.addConverter(Float.class, BigDecimal.class, BigDecimal::valueOf);

        // Double
        registry.addConverter(Double.class, Byte.class, Double::byteValue);
        registry.addConverter(Double.class, Integer.class, Double::intValue);
        registry.addConverter(Double.class, Long.class, Double::longValue);
        registry.addConverter(Double.class, Float.class, Double::floatValue);
        registry.addConverter(Double.class, Short.class, Double::shortValue);
        registry.addConverter(Double.class, BigInteger.class, d -> BigInteger.valueOf(d.longValue()));
        registry.addConverter(Double.class, BigDecimal.class, BigDecimal::valueOf);

        // Big Integer
        registry.addConverter(BigInteger.class, Byte.class, Number::byteValue);
        registry.addConverter(BigInteger.class, Integer.class, Number::intValue);
        registry.addConverter(BigInteger.class, Long.class, Number::longValue);
        registry.addConverter(BigInteger.class, Float.class, Number::floatValue);
        registry.addConverter(BigInteger.class, Short.class, Number::shortValue);
        registry.addConverter(BigInteger.class, Double.class, Number::doubleValue);
        registry.addConverter(BigInteger.class, BigDecimal.class, BigDecimal::new);

        // Big Decimal
        registry.addConverter(BigDecimal.class, Byte.class, Number::byteValue);
        registry.addConverter(BigDecimal.class, Integer.class, Number::intValue);
        registry.addConverter(BigDecimal.class, Long.class, Number::longValue);
        registry.addConverter(BigDecimal.class, Float.class, Number::floatValue);
        registry.addConverter(BigDecimal.class, Short.class, Number::shortValue);
        registry.addConverter(BigDecimal.class, Double.class, Number::doubleValue);
        registry.addConverter(BigDecimal.class, BigInteger.class, BigDecimal::toBigInteger);

        // String
        registry.addConverter(String.class, Boolean.class, Boolean::valueOf);
        registry.addConverter(String.class, Byte.class, Byte::parseByte);
        registry.addConverter(String.class, Integer.class, Integer::parseInt);
        registry.addConverter(String.class, Long.class, Long::parseLong);
        registry.addConverter(String.class, Short.class, Short::parseShort);
        registry.addConverter(String.class, Float.class, Float::parseFloat);
        registry.addConverter(String.class, Double.class, Double::parseDouble);
        registry.addConverter(String.class, BigInteger.class, BigInteger::new);
        registry.addConverter(String.class, BigDecimal.class, BigDecimal::new);

        // TimeZone
        registry.addConverter(String.class, TimeZone.class, CoreConversions::toTimeZone);
        registry.addConverter(ZoneId.class, TimeZone.class, TimeZone::getTimeZone);

        // Long to Date
        registry.addConverter(Long.class, OffsetDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.addConverter(Long.class, ZonedDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()));
        registry.addConverter(Long.class, Date.class, Date::new);
        registry.addConverter(Long.class, Instant.class, Instant::ofEpochMilli);
        registry.addConverter(Long.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s);
            return c;
        });
        registry.addConverter(Long.class, LocalDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.addConverter(Long.class, LocalDate.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDate());
        registry.addConverter(Long.class, LocalTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalTime());

        // Instant
        registry.addConverter(Instant.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.addConverter(Instant.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        registry.addConverter(Instant.class, Date.class, Date::from);
        registry.addConverter(Instant.class, Long.class, Instant::toEpochMilli);
        registry.addConverter(Instant.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s.toEpochMilli());
            return c;
        });
        registry.addConverter(Instant.class, LocalDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.addConverter(Instant.class, LocalDate.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDate());
        registry.addConverter(Instant.class, LocalTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalTime());

        // LocalDateTime
        registry.addConverter(LocalDateTime.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.addConverter(LocalDateTime.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        registry.addConverter(LocalDateTime.class, Instant.class, s -> s.atZone(ZoneId.systemDefault()).toInstant());
        registry.addConverter(LocalDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.atZone(ZoneId.systemDefault())));
        registry.addConverter(LocalDateTime.class, Date.class, s -> Date.from(s.atZone(ZoneId.systemDefault()).toInstant()));
        registry.addConverter(LocalDateTime.class, Long.class, s -> s.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
        registry.addConverter(LocalDateTime.class, LocalDate.class, LocalDateTime::toLocalDate);
        registry.addConverter(LocalDateTime.class, LocalTime.class, LocalDateTime::toLocalTime);

        // ZoneDateTime
        registry.addConverter(ZonedDateTime.class, OffsetDateTime.class, ZonedDateTime::toOffsetDateTime);
        registry.addConverter(ZonedDateTime.class, Instant.class, ZonedDateTime::toInstant);
        registry.addConverter(ZonedDateTime.class, Calendar.class, GregorianCalendar::from);
        registry.addConverter(ZonedDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        registry.addConverter(ZonedDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        registry.addConverter(ZonedDateTime.class, LocalDateTime.class, ZonedDateTime::toLocalDateTime);
        registry.addConverter(ZonedDateTime.class, LocalDate.class, ZonedDateTime::toLocalDate);
        registry.addConverter(ZonedDateTime.class, LocalTime.class, ZonedDateTime::toLocalTime);

        // OffsetDateTime
        registry.addConverter(OffsetDateTime.class, ZonedDateTime.class, OffsetDateTime::toZonedDateTime);
        registry.addConverter(OffsetDateTime.class, Instant.class, OffsetDateTime::toInstant);
        registry.addConverter(OffsetDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.toZonedDateTime()));
        registry.addConverter(OffsetDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        registry.addConverter(OffsetDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        registry.addConverter(OffsetDateTime.class, LocalDateTime.class, OffsetDateTime::toLocalDateTime);
        registry.addConverter(OffsetDateTime.class, LocalDate.class, OffsetDateTime::toLocalDate);
        registry.addConverter(OffsetDateTime.class, LocalTime.class, OffsetDateTime::toLocalTime);

        // Calendar
        registry.addConverter(Calendar.class, Instant.class, Calendar::toInstant);
        registry.addConverter(Calendar.class, Date.class, Calendar::getTime);
        registry.addConverter(Calendar.class, Long.class, Calendar::getTimeInMillis);
        registry.addConverter(Calendar.class, ZonedDateTime.class, SystemConverters::calendarToZonedDateTime);
        registry.addConverter(Calendar.class, OffsetDateTime.class, s -> calendarToZonedDateTime(s).toOffsetDateTime());
        registry.addConverter(Calendar.class, LocalDateTime.class, s -> calendarToZonedDateTime(s).toLocalDateTime());
        registry.addConverter(Calendar.class, LocalDate.class, s -> calendarToZonedDateTime(s).toLocalDate());
        registry.addConverter(Calendar.class, LocalTime.class, s -> calendarToZonedDateTime(s).toLocalTime());

        // Date
        registry.addConverter(Date.class, ZonedDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()));
        registry.addConverter(Date.class, Instant.class, Date::toInstant);
        registry.addConverter(Date.class, OffsetDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.addConverter(Date.class, Long.class, Date::getTime);
        registry.addConverter(Date.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTime(s);
            return c;
        });
        registry.addConverter(Date.class, LocalDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
        registry.addConverter(Date.class, LocalDate.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDate());
        registry.addConverter(Date.class, LocalTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalTime());

        // Object
        registry.addConverter(Object.class, String.class, ConverterDescriptor.Order.MAX, CoreConversions::safeToString);
        registry.addConverter(Object.class, Function.class, ConverterDescriptor.Order.MAX, CoreConversions::toFunction);
        registry.addConverter(Object.class, Predicate.class, ConverterDescriptor.Order.MAX, CoreConversions::toPredicate);
        registry.addConverter(Object.class, CoreLambda.class, ConverterDescriptor.Order.MAX, CoreConversions::toLambda);

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
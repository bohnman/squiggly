package com.github.bohnman.squiggly.core.convert;

import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.function.CoreLambda;
import com.github.bohnman.core.library.CoreLibraries;
import com.github.bohnman.squiggly.core.convert.joda.JodaConverters;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.function.Function;
import java.util.function.Predicate;

public class DefaultConverters {

    public static void add(SquigglyConverterRegistry repo) {

        // Byte
        repo.add(Byte.class, Integer.class, Byte::intValue);
        repo.add(Byte.class, Short.class, Byte::shortValue);
        repo.add(Byte.class, Long.class, Byte::longValue);
        repo.add(Byte.class, Float.class, Byte::floatValue);
        repo.add(Byte.class, Double.class, Byte::doubleValue);
        repo.add(Byte.class, BigInteger.class, (Function<Byte, BigInteger>) BigInteger::valueOf);
        repo.add(Byte.class, BigDecimal.class, BigDecimal::valueOf);

        // Integer
        repo.add(Integer.class, Byte.class, Integer::byteValue);
        repo.add(Integer.class, Short.class, Integer::shortValue);
        repo.add(Integer.class, Long.class, Integer::longValue);
        repo.add(Integer.class, Float.class, Integer::floatValue);
        repo.add(Integer.class, Double.class, Integer::doubleValue);
        repo.add(Integer.class, BigInteger.class, (Function<Integer, BigInteger>) BigInteger::valueOf);
        repo.add(Integer.class, BigDecimal.class, BigDecimal::valueOf);

        // Short
        repo.add(Short.class, Byte.class, Short::byteValue);
        repo.add(Short.class, Integer.class, Short::intValue);
        repo.add(Short.class, Long.class, Short::longValue);
        repo.add(Short.class, Float.class, Short::floatValue);
        repo.add(Short.class, Double.class, Short::doubleValue);
        repo.add(Short.class, BigInteger.class, (Function<Short, BigInteger>) BigInteger::valueOf);
        repo.add(Short.class, BigDecimal.class, BigDecimal::valueOf);

        // Long
        repo.add(Long.class, Byte.class, Long::byteValue);
        repo.add(Long.class, Integer.class, Long::intValue);
        repo.add(Long.class, Short.class, Long::shortValue);
        repo.add(Long.class, Float.class, Long::floatValue);
        repo.add(Long.class, Double.class, Long::doubleValue);
        repo.add(Long.class, BigInteger.class, BigInteger::valueOf);
        repo.add(Long.class, BigDecimal.class, BigDecimal::valueOf);

        // Float
        repo.add(Float.class, Byte.class, Float::byteValue);
        repo.add(Float.class, Integer.class, Float::intValue);
        repo.add(Float.class, Long.class, Float::longValue);
        repo.add(Float.class, Short.class, Float::shortValue);
        repo.add(Float.class, Double.class, Float::doubleValue);
        repo.add(Float.class, BigInteger.class, f -> BigInteger.valueOf(f.longValue()));
        repo.add(Float.class, BigDecimal.class, BigDecimal::valueOf);

        // Double
        repo.add(Double.class, Byte.class, Double::byteValue);
        repo.add(Double.class, Integer.class, Double::intValue);
        repo.add(Double.class, Long.class, Double::longValue);
        repo.add(Double.class, Float.class, Double::floatValue);
        repo.add(Double.class, Short.class, Double::shortValue);
        repo.add(Double.class, BigInteger.class, d -> BigInteger.valueOf(d.longValue()));
        repo.add(Double.class, BigDecimal.class, BigDecimal::valueOf);

        // Big Integer
        repo.add(BigInteger.class, Byte.class, Number::byteValue);
        repo.add(BigInteger.class, Integer.class, Number::intValue);
        repo.add(BigInteger.class, Long.class, Number::longValue);
        repo.add(BigInteger.class, Float.class, Number::floatValue);
        repo.add(BigInteger.class, Short.class, Number::shortValue);
        repo.add(BigInteger.class, Double.class, Number::doubleValue);
        repo.add(BigInteger.class, BigDecimal.class, BigDecimal::new);

        // Big Decimal
        repo.add(BigDecimal.class, Byte.class, Number::byteValue);
        repo.add(BigDecimal.class, Integer.class, Number::intValue);
        repo.add(BigDecimal.class, Long.class, Number::longValue);
        repo.add(BigDecimal.class, Float.class, Number::floatValue);
        repo.add(BigDecimal.class, Short.class, Number::shortValue);
        repo.add(BigDecimal.class, Double.class, Number::doubleValue);
        repo.add(BigDecimal.class, BigInteger.class, BigDecimal::toBigInteger);

        // String
        repo.add(String.class, Boolean.class, Boolean::valueOf);
        repo.add(String.class, Byte.class, Byte::parseByte);
        repo.add(String.class, Integer.class, Integer::parseInt);
        repo.add(String.class, Long.class, Long::parseLong);
        repo.add(String.class, Short.class, Short::parseShort);
        repo.add(String.class, Float.class, Float::parseFloat);
        repo.add(String.class, Double.class, Double::parseDouble);
        repo.add(String.class, BigInteger.class, BigInteger::new);
        repo.add(String.class, BigDecimal.class, BigDecimal::new);

        // TimeZone
        repo.add(String.class, TimeZone.class, CoreConversions::toTimeZone);
        repo.add(ZoneId.class, TimeZone.class, TimeZone::getTimeZone);

        // Long to Date
        repo.add(Long.class, OffsetDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(Long.class, ZonedDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()));
        repo.add(Long.class, Date.class, Date::new);
        repo.add(Long.class, Instant.class, Instant::ofEpochMilli);
        repo.add(Long.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s);
            return c;
        });
        repo.add(Long.class, LocalDateTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDateTime());
        repo.add(Long.class, LocalDate.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalDate());
        repo.add(Long.class, LocalTime.class, s -> Instant.ofEpochMilli(s).atZone(ZoneId.systemDefault()).toLocalTime());

        // Instant
        repo.add(Instant.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(Instant.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        repo.add(Instant.class, Date.class, Date::from);
        repo.add(Instant.class, Long.class, Instant::toEpochMilli);
        repo.add(Instant.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTimeInMillis(s.toEpochMilli());
            return c;
        });
        repo.add(Instant.class, LocalDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDateTime());
        repo.add(Instant.class, LocalDate.class, s -> s.atZone(ZoneId.systemDefault()).toLocalDate());
        repo.add(Instant.class, LocalTime.class, s -> s.atZone(ZoneId.systemDefault()).toLocalTime());

        // LocalDateTime
        repo.add(LocalDateTime.class, OffsetDateTime.class, s -> s.atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(LocalDateTime.class, ZonedDateTime.class, s -> s.atZone(ZoneId.systemDefault()));
        repo.add(LocalDateTime.class, Instant.class, s -> s.atZone(ZoneId.systemDefault()).toInstant());
        repo.add(LocalDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.atZone(ZoneId.systemDefault())));
        repo.add(LocalDateTime.class, Date.class, s -> Date.from(s.atZone(ZoneId.systemDefault()).toInstant()));
        repo.add(LocalDateTime.class, Long.class, s -> s.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
        repo.add(LocalDateTime.class, LocalDate.class, LocalDateTime::toLocalDate);
        repo.add(LocalDateTime.class, LocalTime.class, LocalDateTime::toLocalTime);

        // ZoneDateTime
        repo.add(ZonedDateTime.class, OffsetDateTime.class, ZonedDateTime::toOffsetDateTime);
        repo.add(ZonedDateTime.class, Instant.class, ZonedDateTime::toInstant);
        repo.add(ZonedDateTime.class, Calendar.class, GregorianCalendar::from);
        repo.add(ZonedDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        repo.add(ZonedDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        repo.add(ZonedDateTime.class, LocalDateTime.class, ZonedDateTime::toLocalDateTime);
        repo.add(ZonedDateTime.class, LocalDate.class, ZonedDateTime::toLocalDate);
        repo.add(ZonedDateTime.class, LocalTime.class, ZonedDateTime::toLocalTime);

        // OffsetDateTime
        repo.add(OffsetDateTime.class, ZonedDateTime.class, OffsetDateTime::toZonedDateTime);
        repo.add(OffsetDateTime.class, Instant.class, OffsetDateTime::toInstant);
        repo.add(OffsetDateTime.class, Calendar.class, s -> GregorianCalendar.from(s.toZonedDateTime()));
        repo.add(OffsetDateTime.class, Date.class, s -> Date.from(s.toInstant()));
        repo.add(OffsetDateTime.class, Long.class, s -> s.toInstant().toEpochMilli());
        repo.add(OffsetDateTime.class, LocalDateTime.class, OffsetDateTime::toLocalDateTime);
        repo.add(OffsetDateTime.class, LocalDate.class, OffsetDateTime::toLocalDate);
        repo.add(OffsetDateTime.class, LocalTime.class, OffsetDateTime::toLocalTime);

        // Calendar
        repo.add(Calendar.class, Instant.class, Calendar::toInstant);
        repo.add(Calendar.class, Date.class, Calendar::getTime);
        repo.add(Calendar.class, Long.class, Calendar::getTimeInMillis);
        repo.add(Calendar.class, ZonedDateTime.class, DefaultConverters::calendarToZonedDateTime);
        repo.add(Calendar.class, OffsetDateTime.class, s -> calendarToZonedDateTime(s).toOffsetDateTime());
        repo.add(Calendar.class, LocalDateTime.class, s -> calendarToZonedDateTime(s).toLocalDateTime());
        repo.add(Calendar.class, LocalDate.class, s -> calendarToZonedDateTime(s).toLocalDate());
        repo.add(Calendar.class, LocalTime.class, s -> calendarToZonedDateTime(s).toLocalTime());

        // Date
        repo.add(Date.class, ZonedDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()));
        repo.add(Date.class, Instant.class, Date::toInstant);
        repo.add(Date.class, OffsetDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(Date.class, Long.class, Date::getTime);
        repo.add(Date.class, Calendar.class, s -> {
            Calendar c = Calendar.getInstance();
            c.setTime(s);
            return c;
        });
        repo.add(Date.class, LocalDateTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
        repo.add(Date.class, LocalDate.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalDate());
        repo.add(Date.class, LocalTime.class, s -> s.toInstant().atZone(ZoneId.systemDefault()).toLocalTime());

        if (CoreLibraries.isJodaTimePresent()) {
            JodaConverters.add(repo);
        }

        // Object
        repo.add(Object.class, String.class, CoreConversions::safeToString, ConverterRecord.MAX_ORDER);
        repo.add(Object.class, Function.class, CoreConversions::toFunction, ConverterRecord.MAX_ORDER);
        repo.add(Object.class, Predicate.class, CoreConversions::toPredicate, ConverterRecord.MAX_ORDER);
        repo.add(Object.class, CoreLambda.class, CoreConversions::toLambda, ConverterRecord.MAX_ORDER);

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
package com.github.bohnman.squiggly.convert;

import com.google.common.collect.ImmutableList;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalField;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.function.Function;

public class DefaultConverters {

    public static List<ConverterRecord> get() {
        return ImmutableList.of(
                nullSafe(Integer.class, int.class, Integer::intValue),


                // Byte
                nullSafe(Byte.class, Integer.class, Byte::intValue),
                nullSafe(Byte.class, Short.class, Byte::shortValue),
                nullSafe(Byte.class, Long.class, Byte::longValue),
                nullSafe(Byte.class, Float.class, Byte::floatValue),
                nullSafe(Byte.class, Double.class, Byte::doubleValue),

                // Integer
                nullSafe(Integer.class, Byte.class, Integer::byteValue),
                nullSafe(Integer.class, Short.class, Integer::shortValue),
                nullSafe(Integer.class, Long.class, Integer::longValue),
                nullSafe(Integer.class, Float.class, Integer::floatValue),
                nullSafe(Integer.class, Double.class, Integer::doubleValue),

                // Short
                nullSafe(Short.class, Byte.class, Short::byteValue),
                nullSafe(Short.class, Integer.class, Short::intValue),
                nullSafe(Short.class, Long.class, Short::longValue),
                nullSafe(Short.class, Float.class, Short::floatValue),
                nullSafe(Short.class, Double.class, Short::doubleValue),

                // Long
                nullSafe(Long.class, Byte.class, Long::byteValue),
                nullSafe(Long.class, Integer.class, Long::intValue),
                nullSafe(Long.class, Short.class, Long::shortValue),
                nullSafe(Long.class, Float.class, Long::floatValue),
                nullSafe(Long.class, Double.class, Long::doubleValue),

                // Float
                nullSafe(Float.class, Byte.class, Float::byteValue),
                nullSafe(Float.class, Integer.class, Float::intValue),
                nullSafe(Float.class, Long.class, Float::longValue),
                nullSafe(Float.class, Short.class, Float::shortValue),
                nullSafe(Float.class, Double.class, Float::doubleValue),

                // Double
                nullSafe(Double.class, Byte.class, Double::byteValue),
                nullSafe(Double.class, Integer.class, Double::intValue),
                nullSafe(Double.class, Long.class, Double::longValue),
                nullSafe(Double.class, Float.class, Double::floatValue),
                nullSafe(Double.class, Short.class, Double::shortValue),

                // Object
                nullSafe(Object.class, String.class, Object::toString),

                // String
                nullSafe(String.class, Boolean.class, Boolean::valueOf),
                nullSafe(String.class, Byte.class, Byte::parseByte),
                nullSafe(String.class, Integer.class, Integer::parseInt),
                nullSafe(String.class, Long.class, Long::parseLong),
                nullSafe(String.class, Short.class, Short::parseShort),
                nullSafe(String.class, Float.class, Float::parseFloat),
                nullSafe(String.class, Double.class, Double::parseDouble),

                // Date
                nullSafe(Instant.class, Date.class, Date::from),
                nullSafe(OffsetDateTime.class, Instant.class, OffsetDateTime::toInstant),
                nullSafe(OffsetDateTime.class, Date.class, (d) -> Date.from(d.toInstant())),
                nullSafe(ZonedDateTime.class, Instant.class, ZonedDateTime::toInstant),
                nullSafe(ZonedDateTime.class, Date.class, (d) -> Date.from(d.toInstant()))
        );
    }

    private static <S, T> ConverterRecord nullSafe(Class<S> source, Class<T> target, Function<S, T> converter) {
        return new NullSafeConverterRecord(source, target, converter);
    }

}

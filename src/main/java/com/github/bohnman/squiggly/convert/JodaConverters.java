package com.github.bohnman.squiggly.convert;

import com.google.common.collect.ImmutableList;

import java.util.List;
import java.util.function.Function;

public class JodaConverters {

    public static void add(ConverterRecordRepository repo) {

        /*
        registry.addConverter(new CalendarToReadableInstantConverter());

        registry.addConverter(new DateTimeToCalendarConverter());
        registry.addConverter(new DateTimeToDateConverter());
        registry.addConverter(new DateTimeToDateMidnightConverter());
        registry.addConverter(new DateTimeToInstantConverter());
        registry.addConverter(new DateTimeToLocalDateConverter());
        registry.addConverter(new DateTimeToLocalDateTimeConverter());
        registry.addConverter(new DateTimeToLocalTimeConverter());
        registry.addConverter(new DateTimeToLongConverter());
        registry.addConverter(new DateTimeToMutableDateTimeConverter());

        registry.addConverter(new DateToDateTimeConverter());
        registry.addConverter(new DateToLocalDateConverter());
        registry.addConverter(new DateToLocalDateTimeConverter());
        registry.addConverter(new DateToLocalTimeConverter());
        registry.addConverter(new DateToReadableInstantConverter());

        registry.addConverter(new LocalDateTimeToLocalDateConverter());
        registry.addConverter(new LocalDateTimeToLocalTimeConverter());

        registry.addConverter(new LongToReadableInstantConverter());

        registry.addConverter(new StringToDateTimeConverter());
        registry.addConverter(new StringToDateTimeZoneConverter());
        registry.addConverter(new StringToDayOfWeekConverter());
        registry.addConverter(new StringToLocalDateConverter());
        registry.addConverter(new StringToLocalDateTimeConverter());
        registry.addConverter(new StringToPeriodConverter());
        registry.addConverter(new StringToPeriodMapConverter());
         */
    }

    private static <S, T> ConverterRecord nsAdd(Class<S> source, Class<T> target, Function<S, T> converter) {
        return new NullSafeConverterRecord(source, target, converter);
    }
}

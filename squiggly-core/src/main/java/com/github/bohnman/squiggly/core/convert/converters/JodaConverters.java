package com.github.bohnman.squiggly.core.convert.converters;

import com.github.bohnman.squiggly.core.convert.SquigglyConverterRegistry;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.base.AbstractInstant;

import java.time.*;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Adds ability to convert to and from Joda Time objects.
 */
public class JodaConverters {

    /**
     * Add all Joda converters to the registry.
     *
     * @param registry the registry
     */
    public static void add(SquigglyConverterRegistry registry) {

        registry.add(String.class, DateTimeZone.class, DateTimeZone::forID);
        registry.add(DateTimeZone.class, TimeZone.class, DateTimeZone::toTimeZone);
        registry.add(DateTimeZone.class, ZoneId.class, (s) -> s.toTimeZone().toZoneId());

        registry.add(Date.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.getTime()));
        registry.add(Date.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.getTime()));
        registry.add(Date.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.getTime()).toLocalDateTime());
        registry.add(Date.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.getTime()).toLocalDate());
        registry.add(Date.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.getTime()).toLocalTime());

        registry.add(Calendar.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.getTimeInMillis()));
        registry.add(Calendar.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.getTimeInMillis()));
        registry.add(Calendar.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalDateTime());
        registry.add(Calendar.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalDate());
        registry.add(Calendar.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalTime());

        registry.add(Instant.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toEpochMilli()));
        registry.add(Instant.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toEpochMilli()));
        registry.add(Instant.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.toEpochMilli()).toLocalDateTime());
        registry.add(Instant.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.toEpochMilli()).toLocalDate());
        registry.add(Instant.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.toEpochMilli()).toLocalTime());

        registry.add(Long.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime((long) s));
        registry.add(Long.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant((long) s));
        registry.add(Long.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime((long) s).toLocalDateTime());
        registry.add(Long.class, org.joda.time.LocalDate.class, (s) -> new DateTime((long) s).toLocalDate());
        registry.add(Long.class, org.joda.time.LocalTime.class, (s) -> new DateTime((long) s).toLocalTime());


        registry.add(ZonedDateTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toInstant().toEpochMilli()));
        registry.add(ZonedDateTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toInstant().toEpochMilli()));
        registry.add(ZonedDateTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.toLocalDateTime()));
        registry.add(ZonedDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.toLocalDate()));
        registry.add(ZonedDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.toLocalTime()));

        registry.add(OffsetDateTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toInstant().toEpochMilli()));
        registry.add(OffsetDateTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toInstant().toEpochMilli()));
        registry.add(OffsetDateTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.toLocalDateTime()));
        registry.add(OffsetDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.toLocalDate()));
        registry.add(OffsetDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.toLocalTime()));

        registry.add(LocalDate.class, org.joda.time.LocalDate.class, JodaConverters::toJodaLocalDate);
        registry.add(LocalDate.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.atStartOfDay()));
        registry.add(LocalDate.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.atStartOfDay().toLocalTime()));
        registry.add(LocalDate.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
        registry.add(LocalDate.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));

        registry.add(LocalTime.class, org.joda.time.LocalTime.class, JodaConverters::toJodaLocalTime);
        registry.add(LocalTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.atDate(LocalDate.now())));
        registry.add(LocalTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.atDate(LocalDate.now()).toLocalDate()));
        registry.add(LocalTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.atDate(LocalDate.now()).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
        registry.add(LocalTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.atDate(LocalDate.now()).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));

        registry.add(LocalDateTime.class, org.joda.time.LocalDateTime.class, JodaConverters::toJodaLocalDateTime);
        registry.add(LocalDateTime.class, org.joda.time.DateTime.class, (s) -> toJodaLocalDateTime(s).toDateTime());
        registry.add(LocalDateTime.class, org.joda.time.Instant.class, (s) -> toJodaLocalDateTime(s).toDateTime().toInstant());
        registry.add(LocalDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDateTime(s).toLocalDate());
        registry.add(LocalDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalDateTime(s).toLocalTime());


        registry.add(org.joda.time.LocalDate.class, LocalDate.class, JodaConverters::toLocalDate);
        registry.add(org.joda.time.LocalDate.class, org.joda.time.LocalDateTime.class, (s) -> s.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT));
        registry.add(org.joda.time.LocalDate.class, org.joda.time.DateTime.class, org.joda.time.LocalDate::toDateTimeAtStartOfDay);
        registry.add(org.joda.time.LocalDate.class, ZonedDateTime.class, (s) -> ZonedDateTime.ofInstant(Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()), ZoneId.systemDefault()));
        registry.add(org.joda.time.LocalDate.class, OffsetDateTime.class, (s) -> ZonedDateTime.ofInstant(Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()), ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(org.joda.time.LocalDate.class, org.joda.time.Instant.class, (s) -> s.toDateTimeAtStartOfDay().toInstant());
        registry.add(org.joda.time.LocalDate.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()));
        registry.add(org.joda.time.LocalDate.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT)));
        registry.add(org.joda.time.LocalDate.class, Date.class, (s) -> s.toDateTimeAtStartOfDay().toDate());
        registry.add(org.joda.time.LocalDate.class, Calendar.class, (s) -> s.toDateTimeAtStartOfDay().toCalendar(Locale.getDefault()));
        registry.add(org.joda.time.LocalDate.class, Long.class, (s) -> s.toDateTimeAtStartOfDay().getMillis());
        registry.add(org.joda.time.LocalDate.class, org.joda.time.LocalTime.class, (s) -> org.joda.time.LocalTime.MIDNIGHT);
        registry.add(org.joda.time.LocalDate.class, LocalTime.class, (s) -> toLocalTime(org.joda.time.LocalTime.MIDNIGHT));

        registry.add(org.joda.time.LocalTime.class, LocalTime.class, JodaConverters::toLocalTime);
        registry.add(org.joda.time.LocalTime.class, org.joda.time.LocalDateTime.class, (s) -> s.toDateTimeToday().toLocalDateTime());
        registry.add(org.joda.time.LocalTime.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toDateTimeToday().toLocalDateTime()));
        registry.add(org.joda.time.LocalTime.class, org.joda.time.DateTime.class, org.joda.time.LocalTime::toDateTimeToday);
        registry.add(org.joda.time.LocalTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()).atZone(ZoneId.systemDefault()));
        registry.add(org.joda.time.LocalTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(org.joda.time.LocalTime.class, org.joda.time.Instant.class, (s) -> s.toDateTimeToday().toInstant());
        registry.add(org.joda.time.LocalTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()));
        registry.add(org.joda.time.LocalTime.class, Date.class, (s) -> s.toDateTimeToday().toDate());
        registry.add(org.joda.time.LocalTime.class, Calendar.class, (s) -> s.toDateTimeToday().toCalendar(Locale.getDefault()));
        registry.add(org.joda.time.LocalTime.class, Long.class, (s) -> s.toDateTimeToday().getMillis());
        registry.add(org.joda.time.LocalTime.class, org.joda.time.LocalDate.class, (s) -> s.toDateTimeToday().toLocalDate());
        registry.add(org.joda.time.LocalTime.class, LocalDate.class, (s) -> toLocalDate(s.toDateTimeToday().toLocalDate()));

        registry.add(org.joda.time.LocalDateTime.class, LocalDateTime.class, JodaConverters::toLocalDateTime);
        registry.add(org.joda.time.LocalDateTime.class, org.joda.time.DateTime.class, org.joda.time.LocalDateTime::toDateTime);
        registry.add(org.joda.time.LocalDateTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()).atZone(ZoneId.systemDefault()));
        registry.add(org.joda.time.LocalDateTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(org.joda.time.LocalDateTime.class, org.joda.time.Instant.class, (s) -> s.toDateTime().toInstant());
        registry.add(org.joda.time.LocalDateTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()));
        registry.add(org.joda.time.LocalDateTime.class, Date.class, (s) -> s.toDateTime().toDate());
        registry.add(org.joda.time.LocalDateTime.class, Calendar.class, (s) -> s.toDateTime().toCalendar(Locale.getDefault()));
        registry.add(org.joda.time.LocalDateTime.class, Long.class, (s) -> s.toDateTime().getMillis());
        registry.add(org.joda.time.LocalDateTime.class, org.joda.time.LocalDate.class, org.joda.time.LocalDateTime::toLocalDate);
        registry.add(org.joda.time.LocalDateTime.class, LocalDate.class, (s) -> toLocalDate(s.toDateTime().toLocalDate()));
        registry.add(org.joda.time.LocalDateTime.class, org.joda.time.LocalTime.class, org.joda.time.LocalDateTime::toLocalTime);
        registry.add(org.joda.time.LocalDateTime.class, LocalTime.class, (s) -> toLocalTime(s.toLocalTime()));

        registry.add(org.joda.time.Instant.class, Instant.class, (s) -> Instant.ofEpochMilli(s.getMillis()));
        registry.add(org.joda.time.Instant.class, org.joda.time.DateTime.class, org.joda.time.Instant::toDateTime);
        registry.add(org.joda.time.Instant.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()));
        registry.add(org.joda.time.Instant.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        registry.add(org.joda.time.Instant.class, Date.class, AbstractInstant::toDate);
        registry.add(org.joda.time.Instant.class, Calendar.class, (s) -> s.toDateTime().toCalendar(Locale.getDefault()));
        registry.add(org.joda.time.Instant.class, Long.class, org.joda.time.Instant::getMillis);
        registry.add(org.joda.time.Instant.class, org.joda.time.LocalDateTime.class, (s) -> s.toDateTime().toLocalDateTime());
        registry.add(org.joda.time.Instant.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toDateTime().toLocalDateTime()));
        registry.add(org.joda.time.Instant.class, org.joda.time.LocalDate.class, (s) -> s.toDateTime().toLocalDate());
        registry.add(org.joda.time.Instant.class, LocalDate.class, (s) -> toLocalDate(s.toDateTime().toLocalDate()));
        registry.add(org.joda.time.Instant.class, org.joda.time.LocalTime.class, (s) -> s.toDateTime().toLocalTime());
        registry.add(org.joda.time.Instant.class, LocalTime.class, (s) -> toLocalTime(s.toDateTime().toLocalTime()));

        registry.add(org.joda.time.DateTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(s.getZone().toTimeZone().toZoneId()));
        registry.add(org.joda.time.DateTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(s.getZone().toTimeZone().toZoneId()).toOffsetDateTime());
        registry.add(org.joda.time.DateTime.class, org.joda.time.Instant.class, DateTime::toInstant);
        registry.add(org.joda.time.DateTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.getMillis()));
        registry.add(org.joda.time.DateTime.class, Date.class, DateTime::toDate);
        registry.add(org.joda.time.DateTime.class, Calendar.class, (s) -> s.toCalendar(Locale.getDefault()));
        registry.add(org.joda.time.DateTime.class, Long.class, org.joda.time.DateTime::getMillis);
        registry.add(org.joda.time.DateTime.class, org.joda.time.LocalDateTime.class, DateTime::toLocalDateTime);
        registry.add(org.joda.time.DateTime.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toLocalDateTime()));
        registry.add(org.joda.time.DateTime.class, org.joda.time.LocalDate.class, DateTime::toLocalDate);
        registry.add(org.joda.time.DateTime.class, LocalDate.class, (s) -> toLocalDate(s.toLocalDate()));
        registry.add(org.joda.time.DateTime.class, org.joda.time.LocalTime.class, DateTime::toLocalTime);
        registry.add(org.joda.time.DateTime.class, LocalTime.class, (s) -> toLocalTime(s.toLocalTime()));
    }

    private static LocalDate toLocalDate(org.joda.time.LocalDate localDate) {
        return LocalDate.of(
                localDate.getYear(),
                localDate.getMonthOfYear(),
                localDate.getDayOfMonth()
        );
    }

    private static org.joda.time.LocalDate toJodaLocalDate(LocalDate localDate) {
        return new org.joda.time.LocalDate(
                localDate.getYear(),
                localDate.getMonth().getValue(),
                localDate.getDayOfMonth()
        );
    }

    private static LocalTime toLocalTime(org.joda.time.LocalTime localTime) {
        return LocalTime.of(
                localTime.getHourOfDay(),
                localTime.getMinuteOfHour(),
                localTime.getSecondOfMinute(),
                localTime.getMillisOfSecond() * 1000000
        );
    }

    private static org.joda.time.LocalTime toJodaLocalTime(LocalTime localTime) {
        return new org.joda.time.LocalTime(
                localTime.getHour(),
                localTime.getMinute(),
                localTime.getSecond(),
                (localTime.getNano() / 1000000)
        );
    }

    private static LocalDateTime toLocalDateTime(org.joda.time.LocalDateTime localDateTime) {
        return LocalDateTime.of(
                localDateTime.getYear(),
                localDateTime.getMonthOfYear(),
                localDateTime.getDayOfMonth(),
                localDateTime.getHourOfDay(),
                localDateTime.getMinuteOfHour(),
                localDateTime.getSecondOfMinute(),
                localDateTime.getMillisOfSecond() * 1000000
        );
    }

    private static org.joda.time.LocalDateTime toJodaLocalDateTime(LocalDateTime localDateTime) {


        return new org.joda.time.LocalDateTime(
                localDateTime.getYear(),
                localDateTime.getMonth().getValue(),
                localDateTime.getDayOfMonth(),
                localDateTime.getHour(),
                localDateTime.getMinute(),
                localDateTime.getSecond(),
                (localDateTime.getNano() / 1000000)

        );
    }

}

package com.github.bohnman.squiggly.convert.joda;

import com.github.bohnman.squiggly.convert.ConverterRecordRepository;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.base.AbstractInstant;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public class JodaConverters {

    public static void add(ConverterRecordRepository repo) {

        repo.add(String.class, DateTimeZone.class, DateTimeZone::forID);
        repo.add(DateTimeZone.class, TimeZone.class, DateTimeZone::toTimeZone);
        repo.add(DateTimeZone.class, ZoneId.class, (s) -> s.toTimeZone().toZoneId());

        repo.add(Date.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.getTime()).toLocalDate());
        repo.add(Date.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.getTime()).toLocalTime());
        repo.add(Date.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.getTime()).toLocalDateTime());
        repo.add(Date.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.getTime()));
        repo.add(Date.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.getTime()));

        repo.add(Calendar.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalDate());
        repo.add(Calendar.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalTime());
        repo.add(Calendar.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.getTimeInMillis()).toLocalDateTime());
        repo.add(Calendar.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.getTimeInMillis()));
        repo.add(Calendar.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.getTimeInMillis()));

        repo.add(Instant.class, org.joda.time.LocalDate.class, (s) -> new DateTime(s.toEpochMilli()).toLocalDate());
        repo.add(Instant.class, org.joda.time.LocalTime.class, (s) -> new DateTime(s.toEpochMilli()).toLocalTime());
        repo.add(Instant.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime(s.toEpochMilli()).toLocalDateTime());
        repo.add(Instant.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toEpochMilli()));
        repo.add(Instant.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toEpochMilli()));

        repo.add(Long.class, org.joda.time.LocalDate.class, (s) -> new DateTime((long) s).toLocalDate());
        repo.add(Long.class, org.joda.time.LocalTime.class, (s) -> new DateTime((long) s).toLocalTime());
        repo.add(Long.class, org.joda.time.LocalDateTime.class, (s) -> new DateTime((long) s).toLocalDateTime());
        repo.add(Long.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant((long) s));
        repo.add(Long.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime((long) s));


        repo.add(ZonedDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.toLocalDate()));
        repo.add(ZonedDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.toLocalTime()));
        repo.add(ZonedDateTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.toLocalDateTime()));
        repo.add(ZonedDateTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toInstant().toEpochMilli()));
        repo.add(ZonedDateTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toInstant().toEpochMilli()));

        repo.add(OffsetDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.toLocalDate()));
        repo.add(OffsetDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.toLocalTime()));
        repo.add(OffsetDateTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.toLocalDateTime()));
        repo.add(OffsetDateTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.toInstant().toEpochMilli()));
        repo.add(OffsetDateTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.toInstant().toEpochMilli()));

        repo.add(LocalDate.class, org.joda.time.LocalDate.class, JodaConverters::toJodaLocalDate);
        repo.add(LocalDate.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalTime(s.atStartOfDay().toLocalTime()));
        repo.add(LocalDate.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.atStartOfDay()));
        repo.add(LocalDate.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
        repo.add(LocalDate.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));

        repo.add(LocalTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDate(s.atDate(LocalDate.now()).toLocalDate()));
        repo.add(LocalTime.class, org.joda.time.LocalTime.class, JodaConverters::toJodaLocalTime);
        repo.add(LocalTime.class, org.joda.time.LocalDateTime.class, (s) -> toJodaLocalDateTime(s.atDate(LocalDate.now())));
        repo.add(LocalTime.class, org.joda.time.Instant.class, (s) -> new org.joda.time.Instant(s.atDate(LocalDate.now()).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
        repo.add(LocalTime.class, org.joda.time.DateTime.class, (s) -> new org.joda.time.DateTime(s.atDate(LocalDate.now()).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));

        repo.add(LocalDateTime.class, org.joda.time.LocalDate.class, (s) -> toJodaLocalDateTime(s).toLocalDate());
        repo.add(LocalDateTime.class, org.joda.time.LocalTime.class, (s) -> toJodaLocalDateTime(s).toLocalTime());
        repo.add(LocalDateTime.class, org.joda.time.LocalDateTime.class, JodaConverters::toJodaLocalDateTime);
        repo.add(LocalDateTime.class, org.joda.time.Instant.class, (s) -> toJodaLocalDateTime(s).toDateTime().toInstant());
        repo.add(LocalDateTime.class, org.joda.time.DateTime.class, (s) -> toJodaLocalDateTime(s).toDateTime());


        repo.add(org.joda.time.LocalDate.class, org.joda.time.LocalTime.class, (s) -> org.joda.time.LocalTime.MIDNIGHT);
        repo.add(org.joda.time.LocalDate.class, org.joda.time.LocalDateTime.class, (s) -> s.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT));
        repo.add(org.joda.time.LocalDate.class, org.joda.time.Instant.class, (s) -> s.toDateTimeAtStartOfDay().toInstant());
        repo.add(org.joda.time.LocalDate.class, org.joda.time.DateTime.class, org.joda.time.LocalDate::toDateTimeAtStartOfDay);
        repo.add(org.joda.time.LocalDate.class, Date.class, (s) -> s.toDateTimeAtStartOfDay().toDate());
        repo.add(org.joda.time.LocalDate.class, Calendar.class, (s) -> s.toDateTimeAtStartOfDay().toCalendar(Locale.getDefault()));
        repo.add(org.joda.time.LocalDate.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()));
        repo.add(org.joda.time.LocalDate.class, Long.class, (s) -> s.toDateTimeAtStartOfDay().getMillis());
        repo.add(org.joda.time.LocalDate.class, ZonedDateTime.class, (s) -> ZonedDateTime.ofInstant(Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()), ZoneId.systemDefault()));
        repo.add(org.joda.time.LocalDate.class, OffsetDateTime.class, (s) -> ZonedDateTime.ofInstant(Instant.ofEpochMilli(s.toDateTimeAtStartOfDay().getMillis()), ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(org.joda.time.LocalDate.class, LocalDate.class, JodaConverters::toLocalDate);
        repo.add(org.joda.time.LocalDate.class, LocalTime.class, (s) -> toLocalTime(org.joda.time.LocalTime.MIDNIGHT));
        repo.add(org.joda.time.LocalDate.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toLocalDateTime(org.joda.time.LocalTime.MIDNIGHT)));

        repo.add(org.joda.time.LocalTime.class, org.joda.time.LocalDate.class, (s) -> s.toDateTimeToday().toLocalDate());
        repo.add(org.joda.time.LocalTime.class, org.joda.time.LocalDateTime.class, (s) -> s.toDateTimeToday().toLocalDateTime());
        repo.add(org.joda.time.LocalTime.class, org.joda.time.Instant.class, (s) -> s.toDateTimeToday().toInstant());
        repo.add(org.joda.time.LocalTime.class, org.joda.time.DateTime.class, org.joda.time.LocalTime::toDateTimeToday);
        repo.add(org.joda.time.LocalTime.class, Date.class, (s) -> s.toDateTimeToday().toDate());
        repo.add(org.joda.time.LocalTime.class, Calendar.class, (s) -> s.toDateTimeToday().toCalendar(Locale.getDefault()));
        repo.add(org.joda.time.LocalTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()));
        repo.add(org.joda.time.LocalTime.class, Long.class, (s) -> s.toDateTimeToday().getMillis());
        repo.add(org.joda.time.LocalTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()).atZone(ZoneId.systemDefault()));
        repo.add(org.joda.time.LocalTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTimeToday().getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(org.joda.time.LocalTime.class, LocalDate.class, (s) -> toLocalDate(s.toDateTimeToday().toLocalDate()));
        repo.add(org.joda.time.LocalTime.class, LocalTime.class, JodaConverters::toLocalTime);
        repo.add(org.joda.time.LocalTime.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toDateTimeToday().toLocalDateTime()));

        repo.add(org.joda.time.LocalDateTime.class, org.joda.time.LocalDate.class, org.joda.time.LocalDateTime::toLocalDate);
        repo.add(org.joda.time.LocalDateTime.class, org.joda.time.LocalTime.class, org.joda.time.LocalDateTime::toLocalTime);
        repo.add(org.joda.time.LocalDateTime.class, org.joda.time.Instant.class, (s) -> s.toDateTime().toInstant());
        repo.add(org.joda.time.LocalDateTime.class, org.joda.time.DateTime.class, org.joda.time.LocalDateTime::toDateTime);
        repo.add(org.joda.time.LocalDateTime.class, Date.class, (s) -> s.toDateTime().toDate());
        repo.add(org.joda.time.LocalDateTime.class, Calendar.class, (s) -> s.toDateTime().toCalendar(Locale.getDefault()));
        repo.add(org.joda.time.LocalDateTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()));
        repo.add(org.joda.time.LocalDateTime.class, Long.class, (s) -> s.toDateTime().getMillis());
        repo.add(org.joda.time.LocalDateTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()).atZone(ZoneId.systemDefault()));
        repo.add(org.joda.time.LocalDateTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.toDateTime().getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(org.joda.time.LocalDateTime.class, LocalDate.class, (s) -> toLocalDate(s.toDateTime().toLocalDate()));
        repo.add(org.joda.time.LocalDateTime.class, LocalTime.class, (s) -> toLocalTime(s.toLocalTime()));
        repo.add(org.joda.time.LocalDateTime.class, LocalDateTime.class, JodaConverters::toLocalDateTime);

        repo.add(org.joda.time.Instant.class, org.joda.time.LocalDate.class, (s) -> s.toDateTime().toLocalDate());
        repo.add(org.joda.time.Instant.class, org.joda.time.LocalTime.class, (s) -> s.toDateTime().toLocalTime());
        repo.add(org.joda.time.Instant.class, org.joda.time.LocalDateTime.class, (s) -> s.toDateTime().toLocalDateTime());
        repo.add(org.joda.time.Instant.class, org.joda.time.DateTime.class, org.joda.time.Instant::toDateTime);
        repo.add(org.joda.time.Instant.class, Date.class, AbstractInstant::toDate);
        repo.add(org.joda.time.Instant.class, Calendar.class, (s) -> s.toDateTime().toCalendar(Locale.getDefault()));
        repo.add(org.joda.time.Instant.class, Instant.class, (s) -> Instant.ofEpochMilli(s.getMillis()));
        repo.add(org.joda.time.Instant.class, Long.class, org.joda.time.Instant::getMillis);
        repo.add(org.joda.time.Instant.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()));
        repo.add(org.joda.time.Instant.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(org.joda.time.Instant.class, LocalDate.class, (s) -> toLocalDate(s.toDateTime().toLocalDate()));
        repo.add(org.joda.time.Instant.class, LocalTime.class, (s) -> toLocalTime(s.toDateTime().toLocalTime()));
        repo.add(org.joda.time.Instant.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toDateTime().toLocalDateTime()));

        repo.add(org.joda.time.DateTime.class, org.joda.time.LocalDate.class, DateTime::toLocalDate);
        repo.add(org.joda.time.DateTime.class, org.joda.time.LocalTime.class, DateTime::toLocalTime);
        repo.add(org.joda.time.DateTime.class, org.joda.time.LocalDateTime.class, DateTime::toLocalDateTime);
        repo.add(org.joda.time.DateTime.class, org.joda.time.Instant.class, DateTime::toInstant);
        repo.add(org.joda.time.DateTime.class, Date.class, DateTime::toDate);
        repo.add(org.joda.time.DateTime.class, Calendar.class, (s) -> s.toCalendar(Locale.getDefault()));
        repo.add(org.joda.time.DateTime.class, Instant.class, (s) -> Instant.ofEpochMilli(s.getMillis()));
        repo.add(org.joda.time.DateTime.class, Long.class, org.joda.time.DateTime::getMillis);
        repo.add(org.joda.time.DateTime.class, ZonedDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()));
        repo.add(org.joda.time.DateTime.class, OffsetDateTime.class, (s) -> Instant.ofEpochMilli(s.getMillis()).atZone(ZoneId.systemDefault()).toOffsetDateTime());
        repo.add(org.joda.time.DateTime.class, LocalDate.class, (s) -> toLocalDate(s.toLocalDate()));
        repo.add(org.joda.time.DateTime.class, LocalTime.class, (s) -> toLocalTime(s.toLocalTime()));
        repo.add(org.joda.time.DateTime.class, LocalDateTime.class, (s) -> toLocalDateTime(s.toLocalDateTime()));
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

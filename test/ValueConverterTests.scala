package test

import org.mojoz.querease.ValueConverter
import org.mojoz.querease.ValueConverter._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.sql.{Date, Time, Timestamp}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime, ZoneId}

class ValueConverterTests extends FlatSpec with Matchers {

  val converterPlus = new ValueConverter {
    override lazy val zoneId = ZoneId.of("Asia/Tbilisi")
  }

  val converterMinus = new ValueConverter {
    override lazy val zoneId = ZoneId.of("Brazil/Acre")
  }

  behavior of "ValueConverter"

  it should "convert date" in {
    converterPlus .convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterPlus .convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T20:00:00Z"))
    converterMinus.convertToType(java.sql.Date.valueOf("2024-10-17"),     ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T05:00:00Z"))

    converterPlus .convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterPlus .convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T20:00:00Z"))
    converterMinus.convertToType(LocalDate.parse("2024-10-17"), ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T05:00:00Z"))

    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaSqlDate      ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-16")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z"))
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z")),  ClassOfJavaUtilDate     ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T00:00:00Z"))
  }

  it should "convert datetime" in {
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("01:48:26")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("01:48:26")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 01:48:26")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 01:48:26")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T21:48:26Z")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T06:48:26Z")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T01:48:26")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T01:48:26")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("01:48:26")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("01:48:26")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+04:00")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26-05:00")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26+04:00[Asia/Tbilisi]")
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26-05:00[Brazil/Acre]")
    converterPlus .convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T21:48:26Z"))
    converterMinus.convertToType(java.sql.Timestamp.valueOf("2024-10-17 01:48:26"), ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T06:48:26Z"))

    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("05:48:26")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("20:48:26")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 05:48:26")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 20:48:26")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T01:48:26Z")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T01:48:26Z")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T05:48:26")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T20:48:26")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("05:48:26")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("20:48:26")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T05:48:26+04:00")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-16T20:48:26-05:00")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T05:48:26+04:00[Asia/Tbilisi]")
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-16T20:48:26-05:00[Brazil/Acre]")
    converterPlus .convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z"))
    converterMinus.convertToType(Instant.parse("2024-10-17T01:48:26Z"),             ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z"))

    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("01:48:26")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("01:48:26")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 01:48:26")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 01:48:26")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T21:48:26Z")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T06:48:26Z")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T01:48:26")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T01:48:26")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("01:48:26")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("01:48:26")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+04:00")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26-05:00")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26+04:00[Asia/Tbilisi]")
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26-05:00[Brazil/Acre]")
    converterPlus .convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T21:48:26Z"))
    converterMinus.convertToType(LocalDateTime.parse("2024-10-17T01:48:26"),        ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T06:48:26Z"))

    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("19:48:26")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("10:48:26")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 19:48:26")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 10:48:26")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T15:48:26Z")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T15:48:26Z")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T19:48:26")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T10:48:26")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("19:48:26")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("10:48:26")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+10:00")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+10:00")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-16T19:48:26+04:00[Asia/Tbilisi]")
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-16T10:48:26-05:00[Brazil/Acre]")
    converterPlus .convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T15:48:26Z"))
    converterMinus.convertToType(OffsetDateTime.parse("2024-10-17T01:48:26+10:00"), ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T15:48:26Z"))

    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("19:48:26")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("10:48:26")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 19:48:26")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 10:48:26")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T15:48:26Z")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-16T15:48:26Z")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T19:48:26")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T10:48:26")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("19:48:26")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("10:48:26")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+10:00")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T01:48:26+10:00")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]")
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]")
    converterPlus .convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T15:48:26Z"))
    converterMinus.convertToType(ZonedDateTime.parse("2024-10-17T01:48:26+10:00[Australia/Brisbane]"),  ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-16T15:48:26Z"))

    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlDate           ) shouldBe java.sql.Date.valueOf("2024-10-16")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("05:48:26")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlTime           ) shouldBe java.sql.Time.valueOf("20:48:26")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-17 05:48:26")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaSqlTimestamp      ) shouldBe java.sql.Timestamp.valueOf("2024-10-16 20:48:26")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T01:48:26Z")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeInstant       ) shouldBe Instant.parse("2024-10-17T01:48:26Z")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalDate     ) shouldBe LocalDate.parse("2024-10-16")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-17T05:48:26")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalDateTime ) shouldBe LocalDateTime.parse("2024-10-16T20:48:26")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("05:48:26")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeLocalTime     ) shouldBe LocalTime.parse("20:48:26")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-17T05:48:26+04:00")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeOffsetDateTime) shouldBe OffsetDateTime.parse("2024-10-16T20:48:26-05:00")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-17T05:48:26+04:00[Asia/Tbilisi]")
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaTimeZonedDateTime ) shouldBe ZonedDateTime.parse("2024-10-16T20:48:26-05:00[Brazil/Acre]")
    converterPlus .convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z"))
    converterMinus.convertToType(java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z")),  ClassOfJavaUtilDate          ) shouldBe java.util.Date.from(Instant.parse("2024-10-17T01:48:26Z"))
  }

  it should "convert time" in {
    converterPlus .convertToType(java.sql.Time.valueOf("15:35:59"),     ClassOfJavaSqlTime           )      shouldBe java.sql.Time.valueOf("15:35:59")
    converterMinus.convertToType(java.sql.Time.valueOf("15:35:59"),     ClassOfJavaSqlTime           )      shouldBe java.sql.Time.valueOf("15:35:59")
    converterPlus .convertToType(java.sql.Time.valueOf("15:35:59"),     ClassOfJavaTimeLocalTime     )      shouldBe LocalTime.parse("15:35:59")
    converterMinus.convertToType(java.sql.Time.valueOf("15:35:59"),     ClassOfJavaTimeLocalTime     )      shouldBe LocalTime.parse("15:35:59")

    converterPlus .convertToType(LocalTime.parse("15:35:59"), ClassOfJavaSqlTime           )      shouldBe java.sql.Time.valueOf("15:35:59")
    converterMinus.convertToType(LocalTime.parse("15:35:59"), ClassOfJavaSqlTime           )      shouldBe java.sql.Time.valueOf("15:35:59")
    converterPlus .convertToType(LocalTime.parse("15:35:59"), ClassOfJavaTimeLocalTime     )      shouldBe LocalTime.parse("15:35:59")
    converterMinus.convertToType(LocalTime.parse("15:35:59"), ClassOfJavaTimeLocalTime     )      shouldBe LocalTime.parse("15:35:59")
  }

  it should "parse date" in {
    converterPlus .convertToType("2024-10-17",    ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType("2024-10-17",    ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType("2024-10-17",    ClassOfJavaSqlDate)       shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType("2024-10-17",    ClassOfJavaSqlDate)       shouldBe java.sql.Date.valueOf("2024-10-17")
    converterPlus .convertToType("2024-10-17",    ClassOfJavaUtilDate)      shouldBe java.util.Date.from(Instant.parse("2024-10-16T20:00:00Z"))
    converterMinus.convertToType("2024-10-17",    ClassOfJavaUtilDate)      shouldBe java.util.Date.from(Instant.parse("2024-10-17T05:00:00Z"))
  }

  it should "parse time" in {
    converterPlus .convertToType("01:48",         ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:00")
    converterPlus .convertToType("01:48:44",      ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:44")
    converterPlus .convertToType("01:48:44.1",    ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:44.1")
    converterPlus .convertToType("01:48:44.12",   ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:44.12")
    converterPlus .convertToType("01:48:44.123",  ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:44.123")
    converterPlus .convertToType("01:48:44.1234", ClassOfJavaTimeLocalTime) shouldBe       LocalTime.parse("01:48:44.1234")
    converterPlus .convertToType("01:48",         ClassOfJavaSqlTime)       shouldBe java.sql.Time.valueOf("01:48:00")
    converterPlus .convertToType("01:48:44",      ClassOfJavaSqlTime)       shouldBe java.sql.Time.valueOf("01:48:44")
    converterPlus .convertToType("01:48:44.1",    ClassOfJavaSqlTime)       shouldBe java.sql.Time.valueOf("01:48:44")
    converterPlus .convertToType("01:48:44.12",   ClassOfJavaSqlTime)       shouldBe java.sql.Time.valueOf("01:48:44")
    converterPlus .convertToType("01:48:44.123",  ClassOfJavaSqlTime)       shouldBe java.sql.Time.valueOf("01:48:44")
  }
}

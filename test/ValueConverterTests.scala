package test

import org.mojoz.querease.ValueConverter
import org.mojoz.querease.ValueConverter._
import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers

import java.sql.{Date, Time, Timestamp}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneId}

class ValueConverterTests extends FlatSpec with Matchers {

  val converterPlus = new ValueConverter {
    override lazy val zoneId = ZoneId.of("Europe/Riga")
  }

  val converterMinus = new ValueConverter {
    override lazy val zoneId = ZoneId.of("Brazil/Acre")
  }

  "value converter" should "parse date" in {
    converterPlus .convertToType("2024-10-17",    ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterMinus.convertToType("2024-10-17",    ClassOfJavaTimeLocalDate) shouldBe LocalDate.parse("2024-10-17")
    converterPlus .convertToType("2024-10-17",    ClassOfJavaSqlDate)       shouldBe java.sql.Date.valueOf("2024-10-17")
    converterMinus.convertToType("2024-10-17",    ClassOfJavaSqlDate)       shouldBe java.sql.Date.valueOf("2024-10-17")
  }

  "value converter" should "parse time" in {
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

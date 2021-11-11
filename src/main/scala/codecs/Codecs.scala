package codecs

import cats.data.ValidatedNel
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.validated._
import codecs.Json._

sealed abstract class ReaderError(message: String, field: String)
case class WrongType(field: String = "", message: String = "Wrong field type") extends ReaderError(message, field)
case class AbsentField(field: String = "", message: String = "Absent field") extends ReaderError(message, field)

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {

  implicit val intWriter: JsonWriter[Int] = JsonInt(_)
  implicit val doubleWriter: JsonWriter[Double] = JsonDouble(_)
  implicit val stringWriter: JsonWriter[String] = JsonString(_)

  implicit def optionWriter[A, B](implicit evidence: A <:< B, innerWriter: JsonWriter[B]): JsonWriter[Option[A]] = {
    case Some(value) => innerWriter.write(value)
    case None        => JsonNull
  }

  implicit def listWriter[A, B](implicit evidence: A <:< B, innerWriter: JsonWriter[B]): JsonWriter[List[A]] = values =>
    {
      val arrayValues = values.map((value) => innerWriter.write(value))

      JsonArray(arrayValues)
    }

  // Summoner function
  def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  implicit class JsonWriterOps[A](val a: A) {
    def toJson[B](implicit evidence: A <:< B, writer: JsonWriter[B]): Json = writer.write(a)
  }
}

trait JsonReader[A] {
  def read(json: Json): ValidatedNel[ReaderError, A]
}

object JsonReader {

  // Summoner function
  def apply[A](implicit reader: JsonReader[A]): JsonReader[A] = reader

  implicit def intReader: JsonReader[Int] = {
    case JsonInt(value) => value.validNel
    case _              => WrongType().invalidNel
  }

  implicit def stringReader: JsonReader[String] = {
    case JsonString(value) => value.validNel
    case _                 => WrongType().invalidNel
  }

  implicit def doubleReader: JsonReader[Double] = {
    case JsonDouble(value) => value.validNel
    case _                 => WrongType().invalidNel
  }

  implicit def optionReader[T](implicit innerReader: JsonReader[T]): JsonReader[Option[T]] = {
    case JsonNull => None.validNel
    case json     => json.as[T](innerReader).map(Some(_))
  }

  def objectReader[T](transformation: Map[String, Json] => ValidatedNel[ReaderError, T]): JsonReader[T] = {
    case JsonObject(valueMap) => transformation(valueMap)
    case _                    => WrongType().invalidNel
  }

  private def prependErrorPath(current: String, prefix: String) = current match {
    case _ if current.isEmpty => prefix
    case _                    => s"$prefix.$current"
  }

  implicit def listReader[T](implicit innerReader: JsonReader[T]): JsonReader[List[T]] = {
    case JsonArray(values) =>
      values
        .map(innerReader.read)
        .mapWithIndex((validated, index) =>
          validated.updateErrors(current => prependErrorPath(current, index.toString))
        )
        .sequence
    case _ => WrongType().invalidNel
  }

  implicit private class ErrorUpdatingSyntax[T](val validated: ValidatedNel[ReaderError, T]) {

    def updateErrors(transformation: (String) => String): ValidatedNel[ReaderError, T] = {
      validated.leftMap(errors =>
        errors.map {
          case wt: WrongType   => wt.copy(field = transformation(wt.field))
          case af: AbsentField => af.copy(field = transformation(af.field))
        }
      )
    }
  }

  implicit class ObjectReaderSyntax(val map: Map[String, Json]) {

    def readField[T](fieldName: String)(implicit innerReader: JsonReader[T]): ValidatedNel[ReaderError, T] = {
      map.get(fieldName) match {
        case Some(json) => json.as[T](innerReader).updateErrors(current => prependErrorPath(current, fieldName))
        case None       => AbsentField(fieldName).invalidNel
      }
    }
  }

  implicit class JsonReaderOps(val json: Json) extends AnyVal {
    def as[A](implicit reader: JsonReader[A]): ValidatedNel[ReaderError, A] = reader.read(json)
  }
}

trait Codecs[A] extends JsonWriter[A] with JsonReader[A]

object Codecs {
  // Summon Codecs if it exists in implicit scope
  def apply[A](implicit
    reader: JsonReader[A],
    writer: JsonWriter[A]
  ): Codecs[A] = new Codecs[A] {
    def write(a: A): Json = writer.write(a)

    def read(json: Json): ValidatedNel[ReaderError, A] = reader.read(json)
  }
}

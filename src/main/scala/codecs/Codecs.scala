package codecs

import codecs.Json._

sealed abstract class ReaderError(message: String, field: String)
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError(message, field)
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError(message, field)

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {

  implicit val intWriter: JsonWriter[Int] = JsonInt(_)
  implicit val doubleWriter: JsonWriter[Double] = JsonDouble(_)
  implicit val stringWriter: JsonWriter[String] = JsonString(_)

  implicit def optionWriter[A, B](implicit evidence: A <:< B, innerWriter: JsonWriter[B]): JsonWriter[Option[A]] = {
    case Some(value) => innerWriter.write(value)
    case None => JsonNull
  }

  implicit def listWriter[A, B](implicit evidence: A <:< B, innerWriter: JsonWriter[B]): JsonWriter[List[A]] = values => {
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
  def read(json: Json): Either[List[ReaderError], A]
}

object JsonReader {
  // Summoner function
  def apply[A]: JsonReader[A] = ???

  implicit class JsonReaderOps(val json: Json) extends AnyVal {
    def as[A]: Either[ReaderError, A] = ???
  }
}

trait Codecs[A] extends JsonWriter[A] with JsonReader[A]

object Codecs {
  // Summon Codecs if it exists in implicit scope
  def apply[A]: Codecs[A] = ???
}

package codecs

sealed abstract class ReaderError(message: String, field: String)
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError(message, field)
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError(message, field)

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {
  // Summoner function
  def apply[A]: JsonWriter[A] = ???

  implicit class JsonWriterOps[A](val a: A) {
    def toJson: Json = ???
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

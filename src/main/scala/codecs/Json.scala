package codecs

import cats.Show

sealed trait Json
object Json {
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonDouble(value: Double) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  implicit val show: Show[Json] = new Show[Json] {
    override def show(t: Json): String = ???
  } // or just (t: Json) => ??? (called Single Abstract Method)
}

package codecs

import cats.Show

sealed trait Json
object Json {

  val indentStep = "  "

  private def nextIndent(implicit current: String) = current + indentStep

  final case class JsonString(value: String) extends Json

  final case class JsonInt(value: Int) extends Json

  final case class JsonDouble(value: Double) extends Json

  final case class JsonArray(value: List[Json]) extends Json

  final case class JsonObject(value: Map[String, Json]) extends Json

  final case object JsonNull extends Json

  implicit def show(implicit indent: String = ""): Show[Json] = {
    case JsonNull          => "null"
    case JsonString(value) => s"\"$value\""
    case JsonInt(value)    => value.toString
    case JsonDouble(value) => value.toString
    case JsonArray(values) =>
      values
        .map(show(nextIndent).show)
        .mkString(start = s"[\n$nextIndent", sep = s",\n$nextIndent", end = s"\n$indent]")
    case JsonObject(valuesMap) =>
      valuesMap
        .map({ case (key, value) => s"$nextIndent\"$key\": ${show(nextIndent).show(value)}" })
        .mkString(start = s"{\n", sep = s",\n", end = s"\n$indent}")
  }
}

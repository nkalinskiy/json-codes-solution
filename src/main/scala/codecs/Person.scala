package codecs

import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxTuple4Semigroupal}
import codecs.Json.JsonObject
import codecs.JsonReader.{ObjectReaderSyntax, objectReader}
import codecs.JsonWriter.JsonWriterOps

trait Person {
  def name: String
  def age: Int
}
object Person {

//  import TcImplicits._

  private val personAttributesMap: Person => Map[String, Json] = person =>
    Map(
      "name" -> person.name.toJson,
      "age" -> person.age.toJson
    )
  private val workerAttributesMap: Worker => Map[String, Json] = worker => {
    personAttributesMap(worker) ++ Map("salary" -> worker.salary.toJson)
  }

  trait Worker extends Person {
    def salary: Double
  }

  case class University(name: String, city: String, country: String, qsRank: Int)

  case class Student(name: String, age: Int, university: University) extends Person

  case class Employee(name: String, age: Int, salary: Double) extends Worker

  case class Manager(name: String, age: Int, salary: Double, employees: List[Employee]) extends Worker

  implicit val universityJsonWriter: JsonWriter[University] = university => {
    JsonObject(
      Map(
        "name" -> university.name.toJson,
        "city" -> university.city.toJson,
        "country" -> university.country.toJson,
        "qsRank" -> university.qsRank.toJson
      )
    )
  }

  implicit val universityJsonReader: JsonReader[University] = objectReader(map =>
    (
      map.readField[String]("name"),
      map.readField[String]("city"),
      map.readField[String]("country"),
      map.readField[Int]("qsRank")
      ).mapN(University)
  )

  implicit val studentJsonWriter: JsonWriter[Student] = student => {
    JsonObject(
      personAttributesMap(student) ++ Map("university" -> student.university.toJson)
    )
  }

  implicit val personJsonWriter: JsonWriter[Person] = person => {
    JsonObject(personAttributesMap(person))
  }

  implicit val workerJsonWriter: JsonWriter[Worker] = worker => {
    JsonObject(workerAttributesMap(worker))
  }

  implicit val managerJsonWriter: JsonWriter[Manager] = manager => {
    JsonObject(workerAttributesMap(manager) ++ Map("employees" -> manager.employees.toJson[List[Worker]]))
  }



  implicit val employeeJsonReader: JsonReader[Employee] = objectReader(map =>
    (
      map.readField[String]("name"),
      map.readField[Int]("age"),
      map.readField[Double]("salary")
    ).mapN(Employee)
  )

  implicit val managerJsonReader: JsonReader[Manager] = objectReader(map =>
    (
      map.readField[String]("name"),
      map.readField[Int]("age"),
      map.readField[Double]("salary"),
      map.readField[List[Employee]]("employees")
    ).mapN(Manager)
  )
}
//
//trait PersonFallbackImplicits {
//  implicit def personWriter[X <: Person]: JsonWriter[X] = personJsonWriter
//}
//
//trait WorkerFallbackImplicits extends PersonFallbackImplicits {
//  implicit def workerWriter[X <: Worker]: JsonWriter[X] = workerJsonWriter
//}
//
//object TcImplicits extends WorkerFallbackImplicits {
//  implicit def studentWriter[X <: Student]: JsonWriter[X] = studentJsonWriter
//}

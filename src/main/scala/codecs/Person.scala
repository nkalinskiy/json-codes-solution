package codecs

import codecs.Json.JsonObject
import codecs.JsonWriter.JsonWriterOps
import codecs.Person.{Student, Worker, personJsonWriter, studentJsonWriter, workerJsonWriter}

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



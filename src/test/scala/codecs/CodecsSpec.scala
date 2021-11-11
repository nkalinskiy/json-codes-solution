package codecs

import cats.data.Validated
import cats.data.Validated.Valid
import cats.implicits._
import codecs.Json._
import codecs.JsonReader._
import codecs.JsonWriter._
import codecs.Person._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CodecsSpec extends AnyFlatSpec with Matchers {

  "string" should "be parsed as json value" in {
    "String".toJson shouldEqual JsonString("String")
  }

  "string" should "be read from JsonString" in {
    JsonString("String").as[String] shouldEqual "String".validNel
  }

  "integer" should "be parsed as json value" in {
    1234.toJson shouldEqual JsonInt(1234)
  }

  "integer" should "be read from JsonInt" in {
    JsonInt(1234).as[Int] shouldEqual 1234.validNel
  }

  "double" should "be parsed as json value" in {
    2.5.toJson shouldEqual JsonDouble(2.5)
  }

  "double" should "be read from JsonDouble" in {
    JsonDouble(2.5).as[Double] shouldEqual 2.5.validNel
  }

  "list of strings" should "be parsed as json value" in {
    List("Kek", "Shrek").toJson shouldEqual JsonArray(List(JsonString("Kek"), JsonString("Shrek")))
  }

  "list of strings" should "be read from JsonArray" in {
    JsonArray(List(JsonString("Kek"), JsonString("Shrek"))).as[List[String]] shouldEqual
      List("Kek", "Shrek").validNel
  }

  "list of universities" should "be read from JsonArray" in {
    val innoJson = JsonObject(
      Map(
        "name" -> JsonString("Inno"),
        "city" -> JsonString("Inno"),
        "country" -> JsonString("Russia"),
        "qsRank" -> JsonInt(214)
      )
    )
    val mitJson = JsonObject(
      Map(
        "name" -> JsonString("MIT"),
        "city" -> JsonString("Massachusetts"),
        "country" -> JsonString("USA"),
        "qsRank" -> JsonInt(1)
      )
    )
    JsonArray(List(innoJson, mitJson)).as[List[University]] shouldEqual
      Valid(
        List(
          University("Inno", "Inno", "Russia", 214),
          University("MIT", "Massachusetts", "USA", 1)
        )
      )
  }

  "student" should "be parsed as json object" in {
    Student("Max", 21, University("Inno", "Inno", "Russia", 214)).toJson shouldEqual JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(21),
        "university" -> JsonObject(
          Map(
            "name" -> JsonString("Inno"),
            "city" -> JsonString("Inno"),
            "country" -> JsonString("Russia"),
            "qsRank" -> JsonInt(214)
          )
        )
      )
    )
  }

  "student" should "be parsed as person if there is no codec for student in scope" in {
    case class StudentTest(name: String, age: Int, university: University) extends Person
    StudentTest("Max", 21, University("Inno", "Inno", "Russia", 214)).toJson[Person] shouldEqual JsonObject(
      Map("name" -> JsonString("Max"), "age" -> JsonInt(21))
    )
  }

  "student" should "be pretty printed using Show instance" in {
    val studentJson = Student("Max", 21, University("Inno", "Inno", "Russia", 214)).toJson

    val expected = """{
                     |  "name": "Max",
                     |  "age": 21,
                     |  "university": {
                     |    "name": "Inno",
                     |    "city": "Inno",
                     |    "country": "Russia",
                     |    "qsRank": 214
                     |  }
                     |}""".stripMargin

    studentJson.show shouldEqual expected

  }

  "manager" should "be parsed as json object" in {
    val employees = List(Employee("Andy", 23, 40000))
    Manager("Max", 30, 60000, employees).toJson shouldEqual JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(30),
        "salary" -> JsonDouble(60000),
        "employees" -> JsonArray(
          List(
            JsonObject(
              Map(
                "name" -> JsonString("Andy"),
                "age" -> JsonInt(23),
                "salary" -> JsonDouble(40000)
              )
            )
          )
        )
      )
    )
  }

  "manager" should "be parsed from Json" in {
    JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(30),
        "salary" -> JsonDouble(60000),
        "employees" -> JsonArray(
          List(
            JsonObject(
              Map(
                "name" -> JsonString("Andy"),
                "age" -> JsonInt(23),
                "salary" -> JsonDouble(40000)
              )
            )
          )
        )
      )
    ).as[Manager] shouldEqual Validated.Valid(Manager("Max", 30, 60000, List(Employee("Andy", 23, 40000))))
  }

  "manager json" should "return list of errors if json is invalid" in {
    val errors = List(
      WrongType("age"),
      AbsentField("salary"),
      AbsentField("employees.0.age")
    ) // You can implement your own error adt, just change test to show it in action
    JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonDouble(30),
        "employees" -> JsonArray(
          List(
            JsonObject(
              Map(
                "name" -> JsonString("Andy"),
                "salary" -> JsonDouble(40000)
              )
            )
          )
        )
      )
    ).as[Manager] shouldEqual errors.map(_.invalidNel).sequence
  }

  "nested pbject" should "parse" in {
    case class TestEntity(value: Int, manager: Manager)

    implicit val testEntityJsonReader: JsonReader[TestEntity] =
      objectReader(map =>
        (
          map.readField[Int]("value"),
          map.readField[Manager]("manager")
        ).mapN(TestEntity)
      )

    val result = JsonObject(
      Map(
        "value" -> JsonInt(123),
        "manager" -> JsonObject(
          Map(
            "name" -> JsonString("Max"),
            "age" -> JsonInt(30),
            "salary" -> JsonDouble(60000),
            "employees" -> JsonArray(
              List(
                JsonObject(
                  Map(
                    "name" -> JsonString("Andy"),
                    //                    "age" -> JsonInt(23),
                    "salary" -> JsonDouble(40000)
                  )
                )
              )
            )
          )
        )
      )
    ).as[TestEntity] shouldEqual AbsentField("manager.employees.0.age").invalidNel
  }

}

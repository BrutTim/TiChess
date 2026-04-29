package ch.tichess.view

import org.scalatest.funsuite.AnyFunSuite
import spray.json._

class JsonSupportSpec extends AnyFunSuite with JsonSupport {

  test("MoveRequest serialization and deserialization") {
    val req = MoveRequest("fenstring", "e2 e4")
    val json = req.toJson
    assert(json.convertTo[MoveRequest] == req)
  }

  test("ModelResponse serialization and deserialization") {
    val res1 = ModelResponse(true, Some("fen"), None)
    val json1 = res1.toJson
    assert(json1.convertTo[ModelResponse] == res1)

    val res2 = ModelResponse(false, None, Some("error"))
    val json2 = res2.toJson
    assert(json2.convertTo[ModelResponse] == res2)
  }

  test("CommandRequest serialization and deserialization") {
    val req = CommandRequest("e2 e4")
    val json = req.toJson
    assert(json.convertTo[CommandRequest] == req)
  }

  test("CommandResponse serialization and deserialization") {
    val res = CommandResponse(true, Some("msg"), Some("fen"), false)
    val json = res.toJson
    assert(json.convertTo[CommandResponse] == res)
  }

  test("StateResponse serialization and deserialization") {
    val res = StateResponse(
      "fen", "status", false, false, "Q", "q", List("e4"),
      Map("e2" -> List("e3", "e4")), Some("e2"), Some("e4"), "fastparse", List("fastparse", "regex"),
      Some("g1"), Some("Weiss")
    )
    val json = res.toJson
    assert(json.convertTo[StateResponse] == res)
  }
}

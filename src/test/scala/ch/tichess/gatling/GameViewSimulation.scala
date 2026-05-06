package ch.tichess.gatling

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class GameViewSimulation extends Simulation {

  val httpProtocol = http
    .baseUrl("http://localhost:8080")
    .acceptHeader("application/json")

  val scn = scenario("View Game State")
    .exec(
      http("Get Game State")
        .get("/api/view/game")
        .check(status.is(200))
    )
    .pause(1)

  setUp(
    scn.inject(
      rampUsers(100).during(10.seconds),
      constantUsersPerSec(20).during(20.seconds)
    )
  ).protocols(httpProtocol)
   .assertions(
      global.responseTime.percentile3.lt(500), // p95 < 500ms
      global.failedRequests.percent.lt(1)      // error rate < 1%
   )
}

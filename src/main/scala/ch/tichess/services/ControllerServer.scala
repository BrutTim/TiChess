package ch.tichess.services

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import ch.tichess.controller.Controller
import ch.tichess.model.Fen
import ch.tichess.view.{CommandRequest, CommandResponse, JsonSupport}

import scala.concurrent.Await
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object ControllerServer extends JsonSupport:

  def main(args: Array[String]): Unit =
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "TiChessControllerService")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val modelServiceUrl = ServiceConfig.url("MODEL_SERVICE_URL", "http://localhost:8081")
    val modelService: ModelService = new HttpModelService(modelServiceUrl)
    val port = ServiceConfig.port("CONTROLLER_SERVICE_PORT", 8082)

    val dbType = sys.env.getOrElse("DB_TYPE", "postgres")
    
    val dao: ch.tichess.controller.persistence.GameDao = if (dbType == "mongo") {
      val mongoUri = sys.env.getOrElse("MONGO_URI", "mongodb://localhost:27017")
      import org.mongodb.scala._
      val mongoClient = MongoClient(mongoUri)
      val database = mongoClient.getDatabase("tichess")
      val collection = database.getCollection("games")
      new ch.tichess.controller.persistence.mongo.MongoGameDao(collection)
    } else {
      val dbUrl = sys.env.getOrElse("DB_URL", "jdbc:postgresql://postgres-db:5432/tichess")
      val dbUser = sys.env.getOrElse("DB_USER", "postgres")
      val dbPassword = sys.env.getOrElse("DB_PASSWORD", "password")

      import _root_.slick.jdbc.PostgresProfile
      val db = PostgresProfile.api.Database.forURL(dbUrl, driver = "org.postgresql.Driver", user = dbUser, password = dbPassword)
      val slickDao = new ch.tichess.controller.persistence.slick.SlickGameDao(PostgresProfile)(db)
      // Init schema
      Await.result(slickDao.initSchema(), Duration.Inf)
      slickDao
    }

    // Load initial state
    var appState = Await.result(loadStateFromDb(dao), Duration.Inf)

    val route =
      pathPrefix("api" / "controller") {
        concat(
          post {
            path("update") {
              entity(as[CommandRequest]) { req =>
                onComplete(modelServiceReady(modelService, appState, req.input)) {
                  case Success(res) =>
                    appState = res.state
                    // Save state asynchronously
                    saveStateToDb(dao, appState)
                    complete(CommandResponse(success = true, res.message, Some(Fen.encode(res.game)), res.quit))
                  case Failure(ex) =>
                    complete(CommandResponse(success = false, Some(ex.getMessage), None, false))
                }
              }
            }
          },
          get {
            path("state") {
              complete(StateResponseBuilder.fromAppState(appState))
            }
          }
        )
      }

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Controller service online at http://localhost:$port/")
    Await.result(system.whenTerminated, Duration.Inf)

  private def modelServiceReady(modelService: ModelService, appState: ch.tichess.controller.AppState, input: String)(implicit
      ec: ExecutionContextExecutor
  ) =
    Controller.updateAsync(appState, input, modelService)

  def loadStateFromDb(dao: ch.tichess.controller.persistence.GameDao)(implicit ec: ExecutionContextExecutor): scala.concurrent.Future[ch.tichess.controller.AppState] = {
    dao.load("default").map {
      case Some(record) =>
        ch.tichess.model.Pgn.parse(record.pgn, ch.tichess.model.NotationParsers.default) match {
          case Right(imported) =>
            ch.tichess.controller.AppState(imported.game, startGame = imported.startGame, moveHistory = imported.moves)
          case Left(_) =>
            ch.tichess.model.Fen.parse(record.fen) match {
              case Right(game) => ch.tichess.controller.AppState(game, startGame = game)
              case Left(_) => Controller.initialState
            }
        }
      case None => Controller.initialState
    }
  }

  def saveStateToDb(dao: ch.tichess.controller.persistence.GameDao, state: ch.tichess.controller.AppState)(implicit ec: ExecutionContextExecutor): scala.concurrent.Future[Unit] = {
    val fen = ch.tichess.model.Fen.encode(state.game)
    val pgn = ch.tichess.model.Pgn.encode(state.startGame, state.moveHistory)
    val status = if (state.game.isCheckmate) "checkmate" else if (state.game.isDraw || state.drawAgreed) "draw" else "active"
    val record = ch.tichess.controller.persistence.GameRecord("default", fen, pgn, status)
    dao.load("default").flatMap {
      case Some(_) => dao.update(record)
      case None => dao.save(record)
    }
  }

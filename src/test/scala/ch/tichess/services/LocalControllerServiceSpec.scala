package ch.tichess.services

import org.scalatest.funsuite.AsyncFunSuite
import ch.tichess.controller.{AppState, Command, Controller}
import ch.tichess.model.Game

class LocalControllerServiceSpec extends AsyncFunSuite {

  test("LocalControllerService.update calls Controller.updateAsync") {
    val modelService = new LocalModelService()
    val controllerService = new LocalControllerService(modelService)
    
    val initialState = Controller.initialState
    val futureResult = controllerService.update(initialState, "e2 e4")
    
    futureResult.map { result =>
      assert(result.game != initialState.game)
      assert(result.message.isEmpty)
      assert(!result.quit)
    }
  }
}

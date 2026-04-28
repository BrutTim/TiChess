package ch.tichess.controller.persistence

import ch.tichess.controller.persistence.slick.SlickGameDao
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import _root_.slick.jdbc.H2Profile.api._

class SlickGameDaoSpec extends AsyncWordSpec with Matchers {

  "SlickGameDao" should {
    "save, load, update, and delete game records" in {
      val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
      val dao = new SlickGameDao(_root_.slick.jdbc.H2Profile)(db)
      val record = GameRecord("1", "fen1", "pgn1", "active")
      val updatedRecord = GameRecord("1", "fen2", "pgn2", "draw")

      for {
        _ <- dao.initSchema()
        _ <- dao.save(record)
        loaded <- dao.load("1")
        _ = loaded shouldBe Some(record)
        _ <- dao.update(updatedRecord)
        loadedAfterUpdate <- dao.load("1")
        _ = loadedAfterUpdate shouldBe Some(updatedRecord)
        all <- dao.listAll()
        _ = all should contain(updatedRecord)
        _ <- dao.delete("1")
        loadedAfterDelete <- dao.load("1")
        _ = loadedAfterDelete shouldBe None
      } yield succeed
    }
  }
}

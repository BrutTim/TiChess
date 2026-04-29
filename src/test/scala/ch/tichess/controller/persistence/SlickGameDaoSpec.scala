package ch.tichess.controller.persistence

import ch.tichess.controller.persistence.{ChallengeRecord, ChallengeSeeds}
import ch.tichess.controller.persistence.slick.{SlickChallengeDao, SlickGameDao}
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

  "SlickChallengeDao" should {
    "save, load, update, delete, and seed challenge records" in {
      val db = Database.forURL("jdbc:h2:mem:test2;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
      val dao = new SlickChallengeDao(_root_.slick.jdbc.H2Profile)(db)
      val record = ChallengeRecord("mate", "Mate in one", "6k1/8/6K1/8/8/8/8/R7 w - - 0 1", "a1 a8")
      val updatedRecord = record.copy(name = "Updated mate")

      for {
        _ <- dao.initSchema()
        _ <- dao.save(record)
        loaded <- dao.load("mate")
        _ = loaded shouldBe Some(record)
        _ <- dao.update(updatedRecord)
        loadedAfterUpdate <- dao.load("mate")
        _ = loadedAfterUpdate shouldBe Some(updatedRecord)
        all <- dao.listAll()
        _ = all should contain(updatedRecord)
        _ <- dao.delete("mate")
        loadedAfterDelete <- dao.load("mate")
        _ = loadedAfterDelete shouldBe None
        _ <- dao.seedIfEmpty()
        seeded <- dao.listAll()
        _ = seeded.map(_.id) should contain allElementsOf ChallengeSeeds.defaultLichessPuzzles.map(_.id)
      } yield succeed
    }
  }

  "ChallengeSeeds" should {
    "provide a broader built-in Lichess fallback set" in {
      ChallengeSeeds.defaultLichessPuzzles.size should be >= 8
      ChallengeSeeds.defaultLichessPuzzles.map(_.id).distinct shouldBe ChallengeSeeds.defaultLichessPuzzles.map(_.id)
    }
  }
}

package ch.tichess.controller.persistence

import ch.tichess.controller.persistence.{ChallengeRecord, ChallengeSeeds}
import ch.tichess.controller.persistence.slick.{SlickChallengeDao, SlickGameDao}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import _root_.slick.jdbc.H2Profile.api._

import java.nio.file.Files
import scala.concurrent.Future

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
      ChallengeSeeds.defaultLichessPuzzles.size should be >= 2000
      ChallengeSeeds.defaultLichessPuzzles.map(_.id).distinct shouldBe ChallengeSeeds.defaultLichessPuzzles.map(_.id)
    }

    "fall back to bundled rows when no resource stream is available" in {
      val fallback = ChallengeSeeds.loadDefaultLichessPuzzles(None)

      fallback should have size 12
      fallback.map(_.id) should contain("00008")
    }
  }

  "LichessPuzzleImporter" should {
    "read CSV resources from input streams" in {
      val csv =
        """PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags
          |stream,4k3/P7/8/8/8/8/8/4K3 w - - 0 1,a7a8q e8f7,1200,80,90,1,promotion,https://lichess.org/test,
          |""".stripMargin
      val input = new java.io.ByteArrayInputStream(csv.getBytes(java.nio.charset.StandardCharsets.UTF_8))

      val imported = LichessPuzzleImporter.fromCsvInputStream(input)

      imported should have size 1
      imported.head.id shouldBe "stream"
      imported.head.moves shouldBe "e8 f7"
    }
  }

  "ChallengeDao" should {
    "not seed when records already exist" in {
      final class ExistingChallengeDao extends ChallengeDao {
        var saved = Vector.empty[ChallengeRecord]
        override def save(challenge: ChallengeRecord): Future[Unit] =
          saved = saved :+ challenge
          Future.successful(())
        override def load(id: String): Future[Option[ChallengeRecord]] = Future.successful(None)
        override def update(challenge: ChallengeRecord): Future[Unit] = Future.successful(())
        override def delete(id: String): Future[Unit] = Future.successful(())
        override def listAll(): Future[Seq[ChallengeRecord]] =
          Future.successful(Seq(ChallengeRecord("existing", "Existing", "8/8/8/8/8/8/8/8 w - - 0 1", "a1 a2")))
      }

      val dao = new ExistingChallengeDao
      dao.seedIfEmpty().map { _ =>
        dao.saved shouldBe empty
      }
    }
  }

  "LichessPuzzleImporter" should {
    "read CSV files and keep supported promotion solution moves" in {
      val csv = Files.createTempFile("lichess-puzzles", ".csv")
      val header = "PuzzleId,FEN,Moves,Rating,RatingDeviation,Popularity,NbPlays,Themes,GameUrl,OpeningTags"
      val promotionRow =
        "promo,4k3/P7/8/8/8/8/8/4K3 w - - 0 1,a7a8q e8f7 a8a1q a8a1r a8a1b a8a1n,1200,80,90,1,promotion,https://lichess.org/test,"
      Files.writeString(csv, s"$header\n$promotionRow\n")

      val imported = LichessPuzzleImporter.fromCsvFile(csv.toString)

      imported.map(_.id) shouldBe Seq("promo")
      imported.head.moves shouldBe "e8 f7, a8 a1 q, a8 a1 r, a8 a1 b, a8 a1 n"
    }

    "skip malformed rows, invalid first moves, and empty solutions" in {
      val rows = Seq(
        "too-short,only-two-columns",
        "bad-first,4k3/P7/8/8/8/8/8/4K3 w - - 0 1,a7a",
        "empty-solution,4k3/P7/8/8/8/8/8/4K3 w - - 0 1,a7a8q",
        "bad-promotion,4k3/P7/8/8/8/8/8/4K3 w - - 0 1,a7a8q e8f7 a8a1x"
      )

      val imported = LichessPuzzleImporter.fromCsvRows(rows)

      imported.map(_.id) shouldBe Seq("bad-promotion")
      imported.head.moves shouldBe "e8 f7"
    }
  }
}

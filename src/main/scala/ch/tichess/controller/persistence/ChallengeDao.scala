package ch.tichess.controller.persistence

import scala.concurrent.{ExecutionContext, Future}

case class ChallengeRecord(id: String, name: String, fen: String, moves: String)

trait ChallengeDao {
  def save(challenge: ChallengeRecord): Future[Unit]
  def load(id: String): Future[Option[ChallengeRecord]]
  def update(challenge: ChallengeRecord): Future[Unit]
  def delete(id: String): Future[Unit]
  def listAll(): Future[Seq[ChallengeRecord]]

  def seedIfEmpty()(implicit ec: ExecutionContext): Future[Unit] =
    listAll().flatMap { existing =>
      if existing.nonEmpty then Future.successful(())
      else Future.sequence(ChallengeSeeds.defaultLichessPuzzles.map(save)).map(_ => ())
    }
}

object ChallengeSeeds {
  private val DefaultSeedLimit = 3000
  private val SeedResource = "/lichess_puzzles_seed.csv"

  val legacyDemoIds: Set[String] = Set("back-rank-rook", "scholars-mate", "rook-lift")

  private val fallbackLichessRows: Seq[String] = Seq(
    "00008,r6k/pp2r2p/4Rp1Q/3p4/8/1N1P2R1/PqP2bPP/7K b - - 0 24,f2g3 e6e7 b2b1 b3c1 b1c1 h6c1,1935,76,95,9294,crushing hangingPiece long middlegame,https://lichess.org/787zsVup/black#48,",
    "0000D,5rk1/1p3ppp/pq3b2/8/8/1P1Q1N2/P4PPP/3R2K1 w - - 2 27,d3d6 f8d8 d6d8 f6d8,1414,75,96,36011,advantage endgame short,https://lichess.org/F8M8OS71#53,",
    "0008Q,8/4R3/1p2P3/p4r2/P6p/1P3Pk1/4K3/8 w - - 1 64,e7f7 f5e5 e2f1 e5e6,1385,80,92,763,advantage endgame rookEndgame short,https://lichess.org/MQSyb3KW#127,",
    "0009B,r2qr1k1/b1p2ppp/pp4n1/P1P1p3/4P1n1/B2P2Pb/3NBP1P/RN1QR1K1 b - - 1 16,b6c5 e2g4 h3g4 d1g4,1084,74,88,605,advantage middlegame short,https://lichess.org/4MWQCxQ6/black#32,Kings_Pawn_Game Kings_Pawn_Game_Leonardis_Variation",
    "000Pw,6k1/5p1p/4p3/4q3/3nN3/2Q3P1/PP3P1P/6K1 w - - 2 37,e4d2 d4e2 g1f1 e2c3,1550,75,92,625,crushing endgame fork short,https://lichess.org/au2lCK5o#73,",
    "000Sa,2Q2bk1/5p1p/p5p1/2p3P1/2r1B3/7P/qPQ2P2/2K4R b - - 0 32,c4c2 e4c2 a2a1 c2b1,1582,75,97,1202,advantage endgame short,https://lichess.org/lTTa9lwd/black#64,",
    "000VW,r4r2/1p3pkp/p5p1/3R1N1Q/3P4/8/P1q2P2/3R2K1 b - - 3 25,g6f5 d5c5 c2e4 h5g5 g7h8 g5f6,2861,108,85,314,crushing endgame long,https://lichess.org/e9AY2m5j/black#50,",
    "000Vc,8/8/4k1p1/2KpP2p/5PP1/8/8/8 w - - 0 53,g4h5 g6h5 f4f5 e6e5 f5f6 e5f6,1574,78,75,113,crushing endgame long pawnEndgame,https://lichess.org/l6AejDMO#105,",
    "000Zo,4r3/1k6/pp3r2/1b2P2p/3R1p2/P1R2P2/1P4PP/6K1 w - - 0 35,e5f6 e8e1 g1f2 e1f1,1376,75,86,651,endgame mate mateIn2 operaMate short,https://lichess.org/n8Ff742v#69,",
    "000aY,r4rk1/pp3ppp/2n1b3/q1pp2B1/8/P1Q2NP1/1PP1PP1P/2KR3R w - - 0 15,g5e7 a5c3 b2c3 c6e7,1414,78,75,527,advantage master middlegame short,https://lichess.org/iihZGl6t#29,Benoni_Defense Benoni_Defense_Benoni-Indian_Defense",
    "000h0,5rk1/p5p1/3bpr1p/1Pp4q/3pR3/1P1Q1N2/P4PPP/4R1K1 w - - 4 22,e4e6 f6f3 g2f3 h5h2 g1f1 h2h3 f1e2 h3e6,2071,76,89,191,advantage interference kingsideAttack middlegame veryLong,https://lichess.org/OWe6M5dF#43,",
    "001Wz,4r1k1/5ppp/p1Q2n2/1p1p4/3P4/P5P1/1q3PBP/2R3K1 b - - 0 30,b2c1 g2f1 c1c6,1477,75,94,701,advantage endgame short,https://lichess.org/84RH3LaP/black#60,"
  )

  private[persistence] def loadDefaultLichessPuzzles(stream: Option[java.io.InputStream]): Seq[ChallengeRecord] =
    stream
      .map(stream => LichessPuzzleImporter.fromCsvInputStream(stream, DefaultSeedLimit))
      .filter(_.nonEmpty)
      .getOrElse(LichessPuzzleImporter.fromCsvRows(fallbackLichessRows, DefaultSeedLimit))

  lazy val defaultLichessPuzzles: Seq[ChallengeRecord] =
    loadDefaultLichessPuzzles(Option(getClass.getResourceAsStream(SeedResource)))
}

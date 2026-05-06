package ch.tichess.benchmark

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import ch.tichess.model._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class PgnEncodeBenchmark {

  var game: Game = _
  var moves: Vector[Move] = _

  @Setup
  def setup(): Unit = {
    // Setup a long game sequence to benchmark PGN encoding
    game = Game.initial
    val moveStrs = Vector(
      ("e2", "e4"), ("e7", "e5"),
      ("g1", "f3"), ("b8", "c6"),
      ("f1", "b5"), ("a7", "a6"),
      ("b5", "a4"), ("g8", "f6"),
      ("e1", "g1"), ("f8", "e7"),
      ("f1", "e1"), ("b7", "b5"),
      ("a4", "b3"), ("d7", "d6"),
      ("c2", "c3"), ("c6", "a5"),
      ("b3", "c2"), ("c7", "c5"),
      ("d2", "d4"), ("d8", "c7")
    )
    moves = moveStrs.map { case (f, t) => Move(Pos.fromAlgebraic(f).toOption.get, Pos.fromAlgebraic(t).toOption.get, None) }
  }

  @Benchmark
  def encodePgn(): String = {
    Pgn.encode(game, moves)
  }
}

import munit.Assertions as A

import org.expr.mcdm.ranksfromscores
import org.expr.mcdm.Matrix

class TestRanks extends munit.FunSuite {
  test("ranksfromscores returns the rank of each alternative, not the ordering") {
    // Alternative 2 is best, alternative 3 second, alternative 1 last.
    // The ordering would be (2, 3, 1); the ranks are (3, 1, 2).
    val scores = Array(0.3, 0.9, 0.5)
    A.assert(Matrix.elementwise_equal(ranksfromscores(scores), Array(3.0, 1, 2), 1e-12))
  }

  test("ranksfromscores with reverse = false ranks the smallest score first") {
    val scores = Array(0.3, 0.9, 0.5)
    A.assert(Matrix.elementwise_equal(ranksfromscores(scores, reverse = false), Array(1.0, 3, 2), 1e-12))
  }
}

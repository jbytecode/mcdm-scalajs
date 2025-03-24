import munit.Assertions as A

import org.expr.mcdm.wpm
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestWpm extends munit.FunSuite {
  test("Wpm Example - 1") {
    val decmat = Array(
      Array(3.0, 12.5, 2, 120, 14, 3),
      Array(5.0, 15, 3, 110, 38, 4),
      Array(3.0, 13, 2, 120, 19, 3),
      Array(4.0, 14, 2, 100, 31, 4),
      Array(3.0, 15, 1.5, 125, 40, 4)
    )

    val weights = Array(0.221, 0.159, 0.175, 0.127, 0.117, 0.201)

    val directions =
      Array(Maximize, Minimize, Minimize, Maximize, Minimize, Maximize)

    val result = wpm(decmat, weights, directions)

    val expectedScores = Array(0.7975224331331, 0.7532541470585,
      0.7647463553356, 0.7873956894791, 0.7674278741782)

    val expectedBestIndex = 0

    val expectedOrdering = Array(1, 2, 4, 3, 0)

    A.assert(
      Matrix.elementwise_equal(result.scores, expectedScores, 1e-5)
    )

    A.assertEquals(result.bestIndex, expectedBestIndex)

    expectedOrdering.zip(result.ordering).foreach { case (a, b) =>
      A.assertEquals(a, b)
    }

  }
}

import munit.Assertions as A

import org.expr.mcdm.waspas
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestWaspas extends munit.FunSuite {
  test("Waspas Example - 1") {
    val decmat = Array(
      Array(3.0, 12.5, 2, 120, 14, 3),
      Array(5.0, 15, 3, 110, 38, 4),
      Array(3.0, 13, 2, 120, 19, 3),
      Array(4.0, 14, 2, 100, 31, 4),
      Array(3.0, 15, 1.5, 125, 40, 4)
    )

    val weights = Array(0.221, 0.159, 0.175, 0.127, 0.117, 0.201)

    val fns = Array(Maximize, Minimize, Minimize, Maximize, Minimize, Maximize)

    val result = waspas(decmat, weights, fns)

    val expectedScores = Array(0.805021, 0.775060, 0.770181, 0.796424, 0.788239)

    val expectedNormalizedMatrix = Array(
      Array(0.6, 1.0, 0.75, 0.96, 1.0, 0.75),
      Array(1.0, 0.833333, 0.5, 0.88, 0.368421, 1.0),
      Array(0.6, 0.961538, 0.75, 0.96, 0.736842, 0.75),
      Array(0.8, 0.892857, 0.75, 0.8, 0.451613, 1.0),
      Array(0.6, 0.833333, 1.0, 1.0, 0.35, 1.0)
    )

    val expectedScoresWSM = Array(
        0.8125199999999999,
        0.7968652631578947,
        0.7756151417004049,
        0.8054529953917051,
        0.80905)

    val expectedScoresWPM = Array(
        0.7975224331331252,
        0.7532541470584717,
        0.7647463553356331,
        0.7873956894790834,
        0.7674278741781709)

    val expectedBestIndex = 0

    val expectedOrdering = Array(2, 1, 4, 3, 0)

    A.assert(Matrix.elementwise_equal(result.normalizedDecisionMatrix, expectedNormalizedMatrix, 1e-5))

    A.assert(Matrix.elementwise_equal(result.scoresWSM, expectedScoresWSM, 1e-5))

    A.assert(Matrix.elementwise_equal(result.scoresWPM, expectedScoresWPM, 1e-5))

    A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5))

    A.assertEquals(result.bestIndex, expectedBestIndex)

    expectedOrdering.zip(result.orderings).foreach { case (a, b) => A.assertEquals(a, b) }

  }
}

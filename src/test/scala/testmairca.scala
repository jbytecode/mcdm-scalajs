import munit.Assertions as A

import org.expr.mcdm.mairca
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMairca extends munit.FunSuite {
  test("Mairca Example - 1") {

    val decmat = Array(
        Array(6.952, 8.000, 6.649, 7.268, 8.000, 7.652, 6.316),
        Array(7.319, 7.319, 6.604, 7.319, 8.000, 7.652, 5.313),
        Array(7.000, 7.319, 7.652, 6.952, 7.652, 6.952, 4.642),
        Array(7.319, 6.952, 6.649, 7.319, 7.652, 6.649, 5.000))

    val weights = Array(0.172, 0.165, 0.159, 0.129, 0.112, 0.122, 0.140)

    val fns = Array(Maximize, Maximize, Maximize, Maximize, Maximize, Maximize, Minimize)

    val expected_scores = Array(0.1206454, 0.0806646, 0.1458627, 0.1454237)

    val result = mairca(decmat, weights, fns)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 0.001))

  }
}
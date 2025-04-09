import munit.Assertions as A

import org.expr.mcdm.mabac
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMabac extends munit.FunSuite {
  test("Mabac Example - 1") {

    val decmat = Array(
        Array(2.0, 1, 4, 7, 6, 6, 7, 3000),
        Array(4.0, 1, 5, 6, 7, 7, 6, 3500),
        Array(3.0, 2, 6, 6, 5, 6, 8, 4000),
        Array(5.0, 1, 5, 7, 6, 7, 7, 3000),
        Array(4.0, 2, 5, 6, 7, 7, 6, 3000),
        Array(3.0, 2, 6, 6, 6, 6, 6, 3500))

    val weights = Array(0.293, 0.427, 0.067, 0.027, 0.053, 0.027, 0.053, 0.053)

    val fns = Array(
        Maximize,
        Maximize,
        Maximize,
        Maximize,
        Maximize,
        Maximize,
        Maximize,
        Minimize
    )

    val result = mabac(decmat, weights, fns)

    val expected_scores = Array(-0.31132, -0.10898, 0.20035, 0.04218, 0.34452, 0.20035)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 0.0001),
      "The scores are not equal in the Mabac example 1")
  }

}
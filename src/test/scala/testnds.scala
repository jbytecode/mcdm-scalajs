import munit.Assertions as A

import org.expr.mcdm.nds
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestNds extends munit.FunSuite {
  test("NDS - 1") {
    /*
            cases = [
            1.0 2.0 3.0
            2.0 1.0 3.0
            1.0 3.0 2.0
            4.0 5.0 6.0
        ]


        fns = [maximum, maximum, maximum]

        result = nds(cases, fns)
     */
    val decisionMat = Array(
      Array(1.0, 2.0, 3.0),
      Array(2.0, 1.0, 3.0),
      Array(1.0, 3.0, 2.0),
      Array(4.0, 5.0, 6.0)
    )

    val fns = Array(
      Maximize,
      Maximize,
      Maximize
    )

    val result = nds(
      decisionMat,
      fns
    )

    val expected_scores = Array(0.0, 0.0, 0.0, 3.0)

    A.assert(
      Matrix.elementwise_equal(result.scores, expected_scores, 1e-5),
      "The scores do not match the expected values in nds."
    )
  }

  test("NDS - 2") {

    val decisionMat = Array(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(2.0, 4.0, 6.0, 8.0),
      Array(4.0, 8.0, 12.0, 16.0),
      Array(8.0, 16.0, 24.0, 32.0)
    )

    val fns = Array(
      Minimize,
      Minimize,
      Minimize,
      Minimize
    )

    val expected_scores = Array(3.0, 2.0, 1.0, 0.0)

    val result = nds(
      decisionMat,
      fns
    )

    A.assert(
      Matrix.elementwise_equal(result.scores, expected_scores, 1e-5),
      "The scores do not match the expected values in nds."
    )
  }
}

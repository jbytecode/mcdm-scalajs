import munit.Assertions as A

import org.expr.mcdm.codas
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestCodas extends munit.FunSuite {
  test("Codas Example - 1") {

    val decmat = Array(
      Array(60.000, 0.400, 2540, 500, 990),
      Array(6.350, 0.150, 1016, 3000, 1041),
      Array(6.800, 0.100, 1727.2, 1500, 1676),
      Array(10.000, 0.200, 1000, 2000, 965),
      Array(2.500, 0.100, 560, 500, 915),
      Array(4.500, 0.080, 1016, 350, 508),
      Array(3.000, 0.100, 1778, 1000, 920)
    )

    val weights = Array(0.036, 0.192, 0.326, 0.326, 0.12)

    val fns = Array(Maximize, Minimize, Maximize, Maximize, Maximize)

    val expected_scores = Array(0.512176491, 1.463300035, 1.07153259,
      -0.212467998, -1.851520552, -1.17167677, 0.188656204)

    val result = codas(decmat, weights, fns)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-05), 
    "Scores do not match expected values in Codas")
  }
}

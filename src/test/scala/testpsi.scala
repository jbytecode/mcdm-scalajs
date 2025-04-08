import munit.Assertions as A

import org.expr.mcdm.psi
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestPsi extends munit.FunSuite {
  test("Psi Example - 1") {

    val decmat = Array(
        Array(9.0, 8.0, 7.0),
        Array(7.0, 7.0, 8.0),
        Array(6.0, 9.0, 6.0),
        Array(7.0, 6.0, 6.0)
      ).transpose

    val expected_scores = Array(1.1487059780663555, 1.252775986851622, 1.0884916686098811)

    val w = Array(4.0, 2.0, 6.0, 8.0)

    val fns = Array(Maximize, Maximize, Maximize, Maximize)

    val result = psi(decmat, w, fns)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-05), "Scores do not match in Psi")
    }
}
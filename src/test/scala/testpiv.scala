import munit.Assertions as A

import org.expr.mcdm.piv
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestPiv extends munit.FunSuite {
  test("Piv Example - 1") {

    val decmat = Array(
      Array(60.0, 2.5, 2540.0, 500.0, 990.0),
      Array(6.35, 6.667, 1016.0, 3000.0, 1041.0),
      Array(6.8, 10.0, 1727.0, 1500.0, 1676.0),
      Array(10.0, 5.0, 1000.0, 2000.0, 965.0),
      Array(2.5, 9.8, 560.0, 500.0, 915.0),
      Array(4.5, 12.5, 1016.0, 350.0, 508.0),
      Array(3.0, 10.0, 1778.0, 1000.0, 920.0)
    )

    val weights = Array(0.1761, 0.2042, 0.2668, 0.1243, 0.2286)

    val directions = Array(Maximize, Maximize, Maximize, Maximize, Maximize)

    val expected_scores = Array(0.22086675609968962, 0.35854940101222144,
      0.2734184099704686, 0.4005382183676046, 0.4581157878699193,
      0.43595371718873477, 0.3580651803072459)

    val result = piv(decmat, weights, directions)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-05), "PIV scores do not match expected values")
  }
}

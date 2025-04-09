import munit.Assertions as A

import org.expr.mcdm.cocoso
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestCocoso extends munit.FunSuite {
  test("Cocoso Example - 1") {

    val decmat = Array(
      Array(60.00, 0.40, 2540.00, 500.00, 990.00),
      Array(6.35, 0.15, 1016.00, 3000.00, 1041.00),
      Array(6.80, 0.10, 1727.20, 1500.00, 1676.00),
      Array(10.00, 0.20, 1000.00, 2000.00, 965.00),
      Array(2.50, 0.10, 560.00, 500.00, 915.00),
      Array(4.50, 0.08, 1016.00, 350.00, 508.00),
      Array(3.00, 0.10, 1778.00, 1000.00, 920.00)
    )

    val weights = Array(0.036, 0.192, 0.326, 0.326, 0.120)

    val fns = Array(Maximize, Minimize, Maximize, Maximize, Maximize)

    val expected_scores = Array(2.0413128390265998, 2.787989783418825,
      2.8823497955972495, 2.4160457689259287, 1.2986918936013303,
      1.4431429073391682, 2.519094173200623)

    val result = cocoso(decmat, weights, fns)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-5),
      "Cocoso scores do not match expected values.")
  }
}

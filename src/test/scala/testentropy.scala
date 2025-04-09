import munit.Assertions as A

import org.expr.mcdm.entropy
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestEntropy extends munit.FunSuite {
  test("Entropy Example - 1") {

    val decmat = Array(
      Array(2.0, 4, 3, 5, 4, 3),
      Array(1.0, 1, 2, 1, 2, 2),
      Array(4.0, 5, 6, 5, 5, 6),
      Array(7.0, 6, 6, 7, 6, 6),
      Array(6.0, 7, 5, 6, 7, 6),
      Array(6.0, 7, 6, 7, 7, 6),
      Array(7.0, 6, 8, 7, 6, 6),
      Array(3000.0, 3500.0, 4000.0, 3000.0, 3000.0, 3500.0)
    ).transpose

    val directions = Array(
      Maximize,
      Minimize,
      Maximize,
      Maximize,
      Maximize,
      Maximize
    )

    val expected_weights = Array(0.29967360960, 0.44136733892, 0.07009088720,
      0.02123823711, 0.04902292895, 0.02308037885, 0.04776330969, 0.04776330969)

    // directions array is not used.
    // weights are also empty
    val result = entropy(decmat, directions)

    A.assert(Matrix.elementwise_equal(result.weights, expected_weights, 1e-5), "Weights are not equal in Entropy")
  }
}

import munit.Assertions as A

import org.expr.mcdm.rov
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestRov extends munit.FunSuite {
  test("Rov Example - 1") {
    val mat = Array(
      Array(0.035, 34.5, 847, 1.76, 0.335, 0.5, 0.59, 0.59),
      Array(0.027, 36.8, 834, 1.68, 0.335, 0.665, 0.665, 0.665),
      Array(0.037, 38.6, 808, 2.4, 0.59, 0.59, 0.41, 0.5),
      Array(0.028, 32.6, 821, 1.59, 0.5, 0.59, 0.59, 0.41))

    val w = Array(0.3306, 0.0718, 0.1808, 0.0718, 0.0459, 0.126, 0.126, 0.0472)

    val fns = Array(Minimize, Minimize, Minimize, Minimize, Maximize, Minimize, Minimize, Maximize)

    val expected_scores = Array(
            0.1841453340595497,
            0.26171444444444447,
            0.21331577540106955,
            0.34285244206773624)

    val result1 = rov(mat, w, fns)

    A.assert(Matrix.elementwise_equal(result1.scores, expected_scores, 1e-6), "Rov scores do not match expected values")
  }
}
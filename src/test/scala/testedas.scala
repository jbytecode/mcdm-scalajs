import munit.Assertions as A

import org.expr.mcdm.edas
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestEdas extends munit.FunSuite {
  test("Edas Example - 1") {

    val decmat = Array(
      Array(5000.0, 5, 5300, 450),
      Array(4500.0, 5, 5000, 400),
      Array(4500.0, 4, 4700, 400),
      Array(4000.0, 4, 4200, 400),
      Array(5000.0, 4, 7100, 500),
      Array(5000.0, 5, 5400, 450),
      Array(5500.0, 5, 6200, 500),
      Array(5000.0, 4, 5800, 450)
    )

    val weights = Array(0.25, 0.25, 0.25, 0.25)

    val fns = Array(Maximize, Maximize, Minimize, Minimize)

    val result = edas(decmat, weights, fns)

    val expected_scores = Array(0.759594, 0.886016, 0.697472, 0.739658,
      0.059083, 0.731833, 0.641691, 0.385194)
    
    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 0.0001),
      "Scores do not match the expected values"
    )


  }
}

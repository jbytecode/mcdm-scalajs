import munit.Assertions as A

import org.expr.mcdm.lmaw
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestLmaw extends munit.FunSuite {
  test("LMAW Example - 1") {

    val decmat = Array(
      Array(647.34, 6.24, 49.87, 19.46, 212.58, 6.75),
      Array(115.64, 3.24, 16.26, 9.69, 207.59, 3.00),
      Array(373.61, 5.00, 26.43, 12.00, 184.62, 3.74),
      Array(37.63, 2.48, 2.85, 9.25, 142.50, 3.24),
      Array(858.01, 4.74, 62.85, 45.96, 267.95, 4.00),
      Array(222.92, 3.00, 19.24, 21.46, 221.38, 3.49)
    )

    val weights = Array(0.215, 0.126, 0.152, 0.091, 0.19, 0.226)

    val fns = Array(Maximize, Maximize, Minimize, Minimize, Minimize, Maximize)

    val result = lmaw(decmat, weights, fns)

    val expected_scores = Array(4.839005264308832, 4.679718180594332,
      4.797731427991642, 4.732145373983716, 4.73416833375772, 4.702247270959649)

    val expected_best_index = 0 

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-6),
      s"Expected scores: ${Matrix.prettyPrint(expected_scores)} but got: ${Matrix.prettyPrint(result.scores)}")
    
    A.assertEquals(result.best, expected_best_index,
      s"Expected best index: $expected_best_index but got: ${result.best}")
  }
}

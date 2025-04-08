import munit.Assertions as A

import org.expr.mcdm.ram
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestRam extends munit.FunSuite {
  test("Ram Example - 1") {
    val decmat = Array(
      Array(0.068, 0.066, 0.150, 0.098, 0.156, 0.114, 0.098),
      Array(0.078, 0.076, 0.108, 0.136, 0.082, 0.171, 0.105),
      Array(0.157, 0.114, 0.128, 0.083, 0.108, 0.113, 0.131),
      Array(0.106, 0.139, 0.058, 0.074, 0.132, 0.084, 0.120),
      Array(0.103, 0.187, 0.125, 0.176, 0.074, 0.064, 0.057),
      Array(0.105, 0.083, 0.150, 0.051, 0.134, 0.094, 0.113),
      Array(0.137, 0.127, 0.056, 0.133, 0.122, 0.119, 0.114),
      Array(0.100, 0.082, 0.086, 0.060, 0.062, 0.109, 0.093),
      Array(0.053, 0.052, 0.043, 0.100, 0.050, 0.078, 0.063),
      Array(0.094, 0.074, 0.097, 0.087, 0.080, 0.054, 0.106)
    )

    val weights = Array(0.132, 0.135, 0.138, 0.162, 0.09, 0.223, 0.120)

    val directions = Array(
      Maximize,
      Minimize,
      Minimize,
      Maximize,
      Maximize,
      Maximize,
      Maximize
    )

    val expected_scores = Array(1.433215, 1.439243, 1.435296, 1.432197, 1.42788,
      1.43012, 1.439444, 1.430766, 1.429406, 1.428773)

    val result = ram(decmat, weights, directions)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-05))
  }
}

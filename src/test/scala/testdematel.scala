import munit.Assertions as A

import org.expr.mcdm.dematel
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestDematel extends munit.FunSuite {
  test("Dematel Example - 1") {

    val K = Array(
      Array(0.0, 3.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0),
      Array(3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0),
      Array(4.0, 1.0, 0.0, 2.0, 1.0, 3.0, 1.0, 2.0, 3.0, 2.0),
      Array(4.0, 1.0, 4.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0, 0.0),
        Array(3.0, 2.0, 3.0, 1.0, 0.0, 3.0, 0.0, 2.0, 0.0, 0.0),
        Array(4.0, 1.0, 4.0, 4.0, 0.0, 0.0, 0.0, 1.0, 1.0, 3.0),
        Array(3.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0),
        Array(3.0, 0.0, 4.0, 3.0, 2.0, 3.0, 1.0, 0.0, 0.0, 0.0),
        Array(4.0, 3.0, 2.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 2.0),
        Array(2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0)
    )

    /* 
    result = dematel(K)

    @test isapprox(result.threshold, 0.062945, atol = tol)

    @test isapprox(
        result.c,
        [
            0.3991458,
            0.2261648,
            1.0204318,
            0.7538625,
            0.8096760,
            0.9780926,
            0.2717874,
            0.9455390,
            0.5960514,
            0.2937537,
        ],
        atol = tol,
    )

     */
    val result = dematel(K, Array())

    val expectedc = Array(
      0.3991458,
      0.2261648,
      1.0204318,
      0.7538625,
      0.8096760,
      0.9780926,
      0.2717874,
      0.9455390,
      0.5960514,
      0.2937537
    )

    A.assertEquals(result.c.length, expectedc.length)
    A.assert(Matrix.elementwise_equal(result.c, expectedc, 1e-6))
  }
}

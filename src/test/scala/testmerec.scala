import munit.Assertions as A

import org.expr.mcdm.merec
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMerec extends munit.FunSuite {
  test("Merec Example - 1") {

    val decmat = Array(
      Array(450.0, 10.0, 100.0, 220.0, 5.0),
      Array(8000.0, 9100, 8200, 9300, 8400),
      Array(54.0, 2, 31, 1, 23),
      Array(145.0, 160, 153, 162, 158)
    ).transpose

    val fns = Array(Maximize, Maximize, Minimize, Minimize)

    val expected_weights = Array(
      0.5752216672093823,
      0.01409659116846726,
      0.40156136388773117,
      0.009120377734419302
    )

    val result = merec(decmat, fns)

    A.assert(
      Matrix.elementwise_equal(
        result.weights,
        expected_weights,
        0.0001
      )
    )

  }
}

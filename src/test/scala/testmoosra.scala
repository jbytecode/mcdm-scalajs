import munit.Assertions as A

import org.expr.mcdm.moosra
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMoosra extends munit.FunSuite {
  test("Moosra Example - 1") {

    val decmat = Array(
      Array(25.0, 21, 19, 22),
      Array(65.0, 78, 53, 25),
      Array(7.0, 6, 5, 2),
      Array(20.0, 24, 33, 31)
    ).transpose

    val weights = Array(0.25, 0.25, 0.25, 0.25)

    val fns = Array(Maximize, Maximize, Minimize, Maximize)

    val expectedscores = Array(15.714285714285714, 20.499999999999996, 20.999999999999996, 39.0)

    val expectedranks = Array(4.0, 3, 2, 1)

    val resultLeftRight = moosra(decmat, weights, fns)
    val result = resultLeftRight match {
      case Left(s) => fail(s"Moosra failed: $s")
      case Right(res) => res
    }

    A.assert(
        Matrix.elementwise_equal(
            result.scores,
            expectedscores,
            0.0001
        )
    )

    A.assert(
        Matrix.elementwise_equal(
            result.ranks,
            expectedranks,
            0.0001
        )
    )
  }
}
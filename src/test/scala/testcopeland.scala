import munit.Assertions as A

import org.expr.mcdm.copeland
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix
import org.expr.mcdm.rov

class TestCopeland extends munit.FunSuite {
  test("Copeland Example - 1") {

    val mopa_rank = Array(1.0, 4, 2, 3).reverse
    val moosra_rank = Array(1.0, 2, 3, 4).reverse
    val copras_rank = Array(1.0, 3, 2, 4).reverse
    val saw_rank = Array(1.0, 3, 2, 4).reverse
    val wpm_rank = Array(1.0, 3, 2, 4).reverse
    val rov_rank = Array(4.0, 1, 2, 3).reverse

    val mat = Array(
      mopa_rank,
      moosra_rank,
      copras_rank,
      saw_rank,
      wpm_rank,
      rov_rank
    ).transpose

    val result = copeland(mat)

    val expected = Array(4.0, 2, 3, 1)

    A.assert(
      Matrix.elementwise_equal(result.ranks, expected),
      s"Expected ${expected.mkString(",")} but got ${result.ranks.mkString(",")}"
    )

  }
}

import munit.Assertions as A

import org.expr.mcdm.ocra
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestOcra extends munit.FunSuite {
  test("Ocra Example - 1") {

    val decisionMat = Array(
      Array(8.0, 16.0, 1.5, 1.2, 4200.0, 5.0, 5.0, 314.0, 185.0),
      Array(8.0, 16.0, 1.0, 1.3, 4200.0, 5.0, 4.0, 360.0, 156.0),
      Array(10.1, 16.0, 2.0, 1.3, 4060.0, 5.0, 3.0, 503.0, 160.0),
      Array(10.1, 8.0, 1.0, 1.5, 5070.0, 2.0, 4.0, 525.0, 200.0),
      Array(10.0, 16.0, 2.0, 1.2, 6350.0, 5.0, 3.0, 560.0, 190.0),
      Array(10.1, 16.0, 1.0, 1.2, 5500.0, 2.0, 2.0, 521.0, 159.0),
      Array(10.1, 64.0, 2.0, 1.7, 5240.0, 5.0, 3.0, 770.0, 199.0),
      Array(7.0, 32.0, 1.0, 1.8, 3000.0, 3.0, 4.0, 364.0, 157.0),
      Array(10.1, 16.0, 1.0, 1.3, 3540.0, 5.0, 3.0, 510.0, 171.0),
      Array(9.7, 16.0, 2.0, 1.83, 7500.0, 6.0, 2.0, 550.0, 170.0)
    )

    val weights =
      Array(0.167, 0.039, 0.247, 0.247, 0.116, 0.02, 0.056, 0.027, 0.081)

    val directions = Array(
      Maximize,
      Maximize,
      Maximize,
      Maximize,
      Maximize,
      Maximize,
      Maximize,
      Minimize,
      Minimize
    )

    val expected_scores = Array(0.1439209390821490, 0.0241065507104361,
      0.2734201159562310, 0.0429791654417769, 0.3185195380415760,
      0.0024882426914911, 0.5921715172301160, 0.1139028947061430,
      0.0000000000000000, 0.4787485498471800)

    val result = ocra(
      decisionMat,
      weights,
      directions
    )

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-5),
      "The scores do not match the expected values in ocra."
    )
  }
}

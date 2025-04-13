import munit.Assertions as A

import org.expr.mcdm.aras
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestAras extends munit.FunSuite {
  test("Aras Example - 1") {
    val decmat = Array(
      Array(105000.0, 120000.0, 150000.0, 115000.0, 135000.0),
      Array(105.0, 110.0, 120.0, 105.0, 115.0),
      Array(10.0, 15.0, 12.0, 20.0, 15.0),
      Array(4.0, 4.0, 3.0, 4.0, 5.0),
      Array(300.0, 500.0, 550.0, 600.0, 400.0),
      Array(10.0, 8.0, 12.0, 9.0, 9.0)
    ).transpose

    val functionlist =
      Array(Minimize, Maximize, Minimize, Maximize, Maximize, Minimize)

    val w = Array(0.05, 0.20, 0.10, 0.15, 0.10, 0.40)

    val result = aras(decmat, w, functionlist)

    val expectedExtendMat = Array(
      Array(9.52381e-6, 105.0, 0.1, 4.0, 300.0, 0.1),
      Array(8.33333e-6, 110.0, 0.0666667, 4.0, 500.0, 0.125),
      Array(6.66667e-6, 120.0, 0.0833333, 3.0, 550.0, 0.0833333),
      Array(8.69565e-6, 105.0, 0.05, 4.0, 600.0, 0.111111),
      Array(7.40741e-6, 115.0, 0.0666667, 5.0, 400.0, 0.111111),
      Array(9.52381e-6, 120.0, 0.1, 5.0, 600.0, 0.125)
    )

    val expectedNormalized = Array(
      Array(0.189904, 0.155556, 0.214286, 0.16, 0.101695, 0.152542),
      Array(0.166166, 0.162963, 0.142857, 0.16, 0.169492, 0.190678),
      Array(0.132933, 0.177778, 0.178571, 0.12, 0.186441, 0.127119),
      Array(0.173391, 0.155556, 0.107143, 0.16, 0.20339, 0.169492),
      Array(0.147703, 0.17037, 0.142857, 0.2, 0.135593, 0.169492),
      Array(0.189904, 0.177778, 0.214286, 0.2, 0.20339, 0.190678)
    )

    val expectedOptimalityDegrees =
      Array(0.15722131828198205, 0.17240694154264946, 0.1475508603814606,
        0.1626305159740402, 0.16710087227988196, 0.19308949153998578)

    val expectedScores =
      Array(0.81424068, 0.89288620, 0.76415790, 0.84225462, 0.86540635)

    val expectedRanks = Array(2, 5, 4, 1, 3)

    A.assert(
      Matrix.elementwise_equal(result.extendMat, expectedExtendMat, 1e-5)
    )
    A.assert(
      Matrix.elementwise_equal(result.normalizedMat, expectedNormalized, 1e-5)
    )
    A.assert(
      Matrix.elementwise_equal(
        result.optimality_degrees,
        expectedOptimalityDegrees,
        1e-5
      )
    )
    A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5), 
      "Scores do not match in ARAS result")
    
    A.assert(expectedRanks.zip(result.ranks).forall((x, y) => x == y), 
      "Orderings do not match in ARAS result")
      
  }
}

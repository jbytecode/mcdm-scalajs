import munit.Assertions as A

import org.expr.mcdm.copras
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestCopras extends munit.FunSuite {
  test("Copras Example - 1") {

    val decmat = Array(
      Array(2.50, 240, 57, 45, 1.10, 0.333333),
      Array(2.50, 285, 60, 75, 4.00, 0.428571),
      Array(4.50, 320, 100, 65, 7.50, 1.111111),
      Array(4.50, 365, 100, 90, 7.50, 1.111111),
      Array(5.00, 400, 100, 90, 11.00, 1.111111),
      Array(2.50, 225, 60, 45, 1.10, 0.333333),
      Array(2.50, 270, 57, 60, 4.00, 0.428571),
      Array(4.50, 330, 100, 70, 7.50, 1.111111),
      Array(4.50, 365, 100, 80, 7.50, 1.111111),
      Array(5.00, 380, 110, 65, 8.00, 1.111111),
      Array(2.50, 285, 65, 80, 4.00, 0.400000),
      Array(4.00, 280, 75, 65, 4.00, 0.400000),
      Array(4.50, 365, 102, 95, 7.50, 1.111111),
      Array(4.50, 400, 102, 95, 7.50, 1.111111),
      Array(6.00, 450, 110, 95, 11.00, 1.176471),
      Array(6.00, 510, 110, 105, 11.00, 1.176471),
      Array(6.00, 330, 140, 110, 18.50, 1.395349),
      Array(2.50, 240, 65, 80, 4.00, 0.400000),
      Array(4.00, 280, 75, 75, 4.00, 0.400000),
      Array(4.50, 355, 102, 95, 7.50, 1.111111),
      Array(4.50, 385, 102, 90, 7.50, 1.111111),
      Array(5.00, 385, 114, 95, 7.50, 1.000000),
      Array(6.00, 400, 110, 90, 11.00, 1.000000),
      Array(6.00, 480, 110, 95, 15.00, 1.000000),
      Array(6.00, 440, 140, 100, 18.50, 1.200000),
      Array(6.00, 500, 140, 100, 18.50, 1.200000),
      Array(5.00, 450, 125, 100, 15.00, 1.714286),
      Array(6.00, 500, 150, 125, 18.50, 1.714286),
      Array(6.00, 515, 180, 140, 22.00, 2.307692),
      Array(7.00, 550, 200, 150, 30.00, 2.307692),
      Array(6.00, 500, 180, 140, 15.00, 2.307692),
      Array(6.00, 500, 180, 140, 18.50, 2.307692),
      Array(6.00, 500, 180, 140, 22.00, 2.307692),
      Array(7.00, 500, 180, 140, 30.00, 2.307692),
      Array(7.00, 500, 200, 140, 37.00, 2.307692),
      Array(7.00, 500, 200, 140, 45.00, 2.307692),
      Array(7.00, 500, 200, 140, 55.00, 2.307692),
      Array(7.00, 500, 200, 140, 75.00, 2.307692)
    )

    val weights = Array(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1667)

    val fns = Array(Maximize, Maximize, Maximize, Maximize, Maximize, Minimize)

    val result = copras(decmat, weights, fns)

    val expectedScores = Array(0.44194, 0.44395, 0.41042, 0.44403, 0.48177,
      0.44074, 0.42430, 0.41737, 0.43474, 0.44382, 0.46625, 0.48602, 0.45019,
      0.45825, 0.51953, 0.54265, 0.56134, 0.45588, 0.49532, 0.44788, 0.45014,
      0.48126, 0.51586, 0.56243, 0.58709, 0.60091, 0.51850, 0.61085, 0.65888,
      0.75650, 0.61430, 0.63486, 0.65542, 0.72065, 0.77680, 0.82379, 0.88253,
      1.00000);

    A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5),
      "Scores do not match expected values in Copras example 1")

  }
}

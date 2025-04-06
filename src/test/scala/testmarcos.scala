import munit.Assertions as A

import org.expr.mcdm.marcos
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMarcos extends munit.FunSuite {
  test("Marcos Example - 1") {
    val decmat = Array(
        Array(8.675, 8.433, 8.000, 7.800, 8.025, 8.043),
        Array(8.825, 8.600, 7.420, 7.463, 7.825, 8.229),
        Array(8.325, 7.600, 8.040, 7.700, 7.925, 7.600),
        Array(8.525, 8.667, 7.180, 7.375, 7.750, 8.071))

    val weights = Array(0.19019, 0.15915, 0.19819, 0.19019, 0.15115, 0.11111)

    val fns = Array(Maximize, Maximize, Maximize, Maximize, Maximize, Maximize)

    val expected_scores = Array(0.684865943528, 0.672767106696, 0.662596906139, 0.661103207660)

    //val result = marcos(decmat, weights, fns)

    println("*****************************")
    println("Marcos not implemented yet!")
    println("*****************************")
    //A.assert(Matrix.elementwise_equal(result, expected_scores, 1e-6), "Marcos scores do not match expected values")
  }
}
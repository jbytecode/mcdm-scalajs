import munit.Assertions as A

import org.expr.mcdm.cilos
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestCilos extends munit.FunSuite {
  test("Cilos Example - 1") {

    val eps = 0.001 
    
    val decmat = Array(
        Array(3.0, 100.0, 10.0, 7.0),
        Array(2.5, 80.0, 8.0, 5.0),
        Array(1.8, 50.0, 20.0, 11.0),
        Array(2.2, 70.0, 12.0, 9.0))
        

    val expected_weights = Array(0.3343, 0.2199, 0.1957, 0.2501)


    val expected_normalized_matrix = Array(
        Array(0.191, 0.333, 0.279, 0.219),
        Array(0.229, 0.267, 0.349, 0.156),
        Array(0.319, 0.167, 0.140, 0.344),
        Array(0.261, 0.233, 0.233, 0.281)
    )

    val directions = Array(Minimize, Maximize, Minimize, Maximize)

    val result = cilos(decmat, directions)

    A.assert(
        Matrix.elementwise_equal(
            result.normalizedmatrix,
            expected_normalized_matrix,
            eps), "Normalized matrix does not match in Cilos Example - 1")
            
    A.assert(
        Matrix.elementwise_equal(
            result.weights,
            expected_weights,
            eps), "Weights do not match in Cilos Example - 1")

  }
}
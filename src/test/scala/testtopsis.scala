import munit.Assertions as A 


import org.expr.mcdm.topsis
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix


class TestTopsis extends munit.FunSuite {
    test("Simple Topsis Example - 1"){
        val decmat = Array(
            Array(9.0, 8.0, 7.0),
            Array(7.0, 7.0, 8.0),
            Array(6.0, 9.0, 6.0),
            Array(7.0, 6.0, 6.0)).transpose


        val weights = Array(4.0, 2.0, 6.0, 8.0)

        val fns = Array(Maximize, Maximize, Maximize, Maximize)

        val result = topsis(decmat, weights, fns)


        val expectedScores = Array(0.3876870, 0.6503238, 0.0834767)

        A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5))
    }

    
    test("Simple Topsis Example - 2 -> Reversed Directions"){
        val decmat = Array(
            Array(9.0, 8.0, 7.0),
            Array(7.0, 7.0, 8.0),
            Array(6.0, 9.0, 6.0),
            Array(7.0, 6.0, 6.0)).transpose


        val weights = Array(4.0, 2.0, 6.0, 8.0)

        val fns = Array(Maximize, Minimize, Maximize, Minimize)

        val result = topsis(decmat, weights, fns)

        val expectedScores = Array(0.267953,  0.852471,  0.31626)

        A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5))
    }
}
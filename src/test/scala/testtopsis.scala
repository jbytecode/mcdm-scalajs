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


        val weights = Array(4.0, 2.0, 6.0, 8.0).map(_ / 20.0)

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


        val weights = Array(4.0, 2.0, 6.0, 8.0).map(_ / 20.0)

        val fns = Array(Maximize, Minimize, Maximize, Minimize)

        val result = topsis(decmat, weights, fns)

        val expectedScores = Array(0.267953,  0.852471,  0.31626)

        A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5))
    }
    test("Simple Topsis Example - 3"){
        
        val decmat = Array(
            Array(0.616812,  0.415094 ,  0.0168757,  0.338965 ,  0.190954),
            Array(0.938958,  0.363936 ,  0.528116 ,  0.739575 ,  0.669537),
            Array(0.816357,  0.538917 ,  0.306811 ,  0.356321 ,  0.380972),
            Array(0.874629,  0.0295879,  0.340564 ,  0.0345955,  0.291483)
        )

        val weights = Array(0.2, 0.2, 0.2, 0.2, 0.2)
        
        val directions = Array(Maximize, Maximize, Minimize, Minimize, Maximize)
        
        val result = topsis(decmat, weights, directions)
        
        val expectedScores = Array(0.583686201899153, 0.401471600996117, 0.5763739876858547, 0.4825275745637639)
        
        A.assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-5))
    }
}
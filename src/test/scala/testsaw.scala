import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction
import org.expr.mcdm.saw

class TestSaw extends munit.FunSuite {
    
    test("Saw Example - 1"){
        val decmat = Array(
            Array(25.0, 21, 19, 22),
            Array(65.0, 78, 53, 25),
            Array(7.0 ,  6, 5 ,  2),
            Array(20.0, 24, 33, 31)).transpose

        val weights = Array(0.25, 0.25, 0.25, 0.25)

        val directions = Array(
            Direction.Maximize,
            Direction.Maximize,
            Direction.Minimize,
            Direction.Maximize
        )

        val result = saw(decmat, weights, directions)

        val expectedScores = Array(0.681277, 0.725151, 0.709871, 0.784976)

        A.assert(Matrix.elementwise_equal(result.scores, expectedScores))

        A.assertEquals(result.bestIndex, 3)
    }

    test("Saw Example - 2"){
        
        val decmat = Array(
            Array(4.0, 7.0, 3.0, 2.0, 2.0, 2.0, 2.0),
            Array(4.0, 4.0, 6.0, 4.0, 4.0, 3.0, 7.0),
            Array(7.0, 6.0, 4.0, 2.0, 5.0, 5.0, 3.0),
            Array(3.0, 2.0, 5.0, 3.0, 3.0, 2.0, 5.0),
            Array(4.0, 2.0, 2.0, 5.0, 5.0, 3.0, 6.0))
        
        val weights = Array(0.283, 0.162, 0.162, 0.07, 0.085, 0.162, 0.076)

        val directions = Array.fill(7)(Direction.Maximize)

        val result = saw(decmat, weights, directions)

        val expectedScores = Array(0.553228, 0.713485, 0.837428, 0.514657, 0.579342)

        A.assert(Matrix.elementwise_equal(result.scores, expectedScores))
    }

}    
    
import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction
import org.expr.mcdm.gini

class TestGini extends munit.FunSuite {

    test("Gini Example - 1"){
        val decmat = Array(
            Array(1.0, 2.0),
            Array(2.0, 2.0),
            Array(3.0, 8.0)
        )
        val directions = Array(Direction.Maximize, Direction.Maximize)

        val result = gini(decmat, directions)

        A.assert(Matrix.elementwise_equal(result.weights, Array(0.4, 0.6), 1e-8),
            "Expected weights do not match the calculated weights in the Gini method.")
    }

}

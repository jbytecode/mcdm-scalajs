import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction
import org.expr.mcdm.critic

class TestCritic extends munit.FunSuite {
    
    test("Critic Example - 1"){

        val decmat = Array(
            Array(12.9918,  0.7264,  -1.1009, 1.598139592),
            Array(4.1201 , 5.8824 , 3.4483  , 1.021563567),
            Array(4.1039 , 0.0000 , -0.5076 , 0.984469444)
        )

        val directions = Array(
            Direction.Maximize,
            Direction.Maximize,
            Direction.Minimize,
            Direction.Maximize
        )

        val result = critic(decmat, directions)

        val expectedWeights = Array(0.16883925, 0.418444976, 0.249124763, 0.163591012)

        A.assert(Matrix.elementwise_equal(result.weights, expectedWeights, 1e-5), 
            "Expected weights do not match the calculated weights in the Critic method.")
    }

}
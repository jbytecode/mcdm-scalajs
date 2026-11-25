import org.expr.mcdm.ervd
import org.expr.mcdm.Direction.Maximize
import org.expr.mcdm.Matrix

// Test data from JMcDM's testervd.jl (Shyur et al., 2015)
class TestErvd extends munit.FunSuite {
    test("Ervd - Shyur et al. example") {
        val decmat = Array(
            Array(80.0, 70.0, 87.0, 77.0, 76.0, 80.0, 75.0),
            Array(85.0, 65.0, 76.0, 80.0, 75.0, 65.0, 75.0),
            Array(78.0, 90.0, 72.0, 80.0, 85.0, 90.0, 85.0),
            Array(75.0, 84.0, 69.0, 85.0, 65.0, 65.0, 70.0),
            Array(84.0, 67.0, 60.0, 75.0, 85.0, 75.0, 80.0),
            Array(85.0, 78.0, 82.0, 81.0, 79.0, 80.0, 80.0),
            Array(77.0, 83.0, 74.0, 70.0, 71.0, 65.0, 70.0),
            Array(78.0, 82.0, 72.0, 80.0, 78.0, 70.0, 60.0),
            Array(85.0, 90.0, 80.0, 88.0, 90.0, 80.0, 85.0),
            Array(89.0, 75.0, 79.0, 67.0, 77.0, 70.0, 75.0),
            Array(65.0, 55.0, 68.0, 62.0, 70.0, 50.0, 60.0),
            Array(70.0, 64.0, 65.0, 65.0, 60.0, 60.0, 65.0),
            Array(95.0, 80.0, 70.0, 75.0, 70.0, 75.0, 75.0),
            Array(70.0, 80.0, 79.0, 80.0, 85.0, 80.0, 70.0),
            Array(60.0, 78.0, 87.0, 70.0, 66.0, 70.0, 65.0),
            Array(92.0, 85.0, 88.0, 90.0, 85.0, 90.0, 95.0),
            Array(86.0, 87.0, 80.0, 70.0, 72.0, 80.0, 85.0)
        )
        val weights = Array(0.066, 0.196, 0.066, 0.130, 0.130, 0.216, 0.196)
        val directions = Array.fill(7)(Maximize)
        val refs = Array.fill(7)(80.0)

        val result = ervd(decmat, weights, directions, Some(refs))
        val tol = 0.0006

        val expectedScores = Array(0.660, 0.503, 0.885, 0.521, 0.610, 0.796, 0.498, 0.549, 0.908,
            0.565, 0.070, 0.199, 0.632, 0.716, 0.438, 0.972, 0.767)
        val expectedRanks = Array(7, 13, 3, 12, 9, 4, 14, 11, 2, 10, 17, 16, 8, 6, 15, 1, 5).map(_.toDouble)

        assert(Matrix.elementwise_equal(result.normalizedReferencePoints,
            Array(0.059, 0.061, 0.062, 0.062, 0.062, 0.064, 0.063), tol))
        assert(Matrix.elementwise_equal(result.idealDesired,
            Array(0.019, 0.014, 0.011, 0.014, 0.014, 0.014, 0.020), tol))
        assert(Matrix.elementwise_equal(result.idealUndesired,
            Array(-0.055, -0.069, -0.058, -0.052, -0.058, -0.085, -0.058), tol))
        assert(Matrix.elementwise_equal(result.scores, expectedScores, tol))
        assert(Matrix.elementwise_equal(result.ranks, expectedRanks, 1e-9))
    }
}

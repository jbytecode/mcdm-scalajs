import org.expr.mcdm.aroman
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

// Test data from JMcDM's testaroman.jl (Bošković et al., 2023)
class TestAroman extends munit.FunSuite {
    test("Aroman - electric vehicle selection") {
        val decmat = Array(
            Array(40000.0, 1200.0, 1.4, 8.0, 9.0),
            Array(38500.0, 1150.0, 1.2, 6.0, 6.0),
            Array(39400.0, 600.0, 1.1, 7.0, 5.0),
            Array(48000.0, 1300.0, 1.6, 10.0, 12.0)
        )
        val weights = Array(0.28, 0.22, 0.26, 0.15, 0.09)
        val directions = Array(Minimize, Maximize, Maximize, Maximize, Maximize)

        val result = aroman(decmat, weights, directions)
        val tol = 1e-4

        assert(Matrix.elementwise_equal(result.linearNormalizedMatrix(0), Array(0.1579, 0.8571, 0.6, 0.5, 0.5714), tol))
        assert(Matrix.elementwise_equal(result.vectorNormalizedMatrix(0), Array(0.4802, 0.5470, 0.5228, 0.5070, 0.5322), tol))
        assert(Matrix.elementwise_equal(result.minimumSums, Array(0.0447, 0.0324, 0.0397, 0.1103), tol))
        assert(Matrix.elementwise_equal(result.maximumSums, Array(0.2128, 0.1396, 0.0744, 0.2912), tol))
        assert(Matrix.elementwise_equal(result.scores, Array(0.6727, 0.5535, 0.4721, 0.8718), tol))
        assert(Matrix.elementwise_equal(result.ranks, Array(2.0, 3.0, 4.0, 1.0), 1e-9))
    }
}

import org.expr.mcdm.rim
import org.expr.mcdm.Direction.Maximize
import org.expr.mcdm.Matrix

// Test data from JMcDM's testrim.jl (Cables, Lamata and Verdegay, 2016)
class TestRim extends munit.FunSuite {
    val decmat = Array(
        Array(30.0, 0.0, 2.0, 3.0, 3.0, 2.0),
        Array(40.0, 9.0, 1.0, 3.0, 2.0, 2.0),
        Array(25.0, 0.0, 3.0, 1.0, 3.0, 2.0),
        Array(27.0, 0.0, 5.0, 3.0, 3.0, 1.0),
        Array(45.0, 15.0, 2.0, 2.0, 3.0, 4.0)
    )
    val weights = Array(0.2262, 0.2143, 0.1786, 0.1429, 0.1190, 0.1190)
    val ranges = Array((23.0, 60.0), (0.0, 15.0), (0.0, 10.0), (1.0, 3.0), (1.0, 3.0), (1.0, 5.0))
    val ideals = Array((30.0, 35.0), (10.0, 15.0), (0.0, 0.0), (3.0, 3.0), (3.0, 3.0), (4.0, 5.0))
    val directions = Array.fill(6)(Maximize)

    test("Rim - reference ideal intervals") {
        val result = rim(decmat, weights, directions, Some(ranges), Some(ideals))
        val tol = 1e-5

        assert(Matrix.elementwise_equal(result.distanceToIdeal, Array(0.23129, 0.11251, 0.31877, 0.27831, 0.12070), tol))
        assert(Matrix.elementwise_equal(result.distanceToOrigin, Array(0.32823, 0.34831, 0.18852, 0.24344, 0.34378), tol))
        assert(Matrix.elementwise_equal(result.scores, Array(0.58663, 0.75584, 0.37163, 0.46658, 0.74015), tol))
        assert(Matrix.elementwise_equal(result.ranks, Array(3.0, 1.0, 5.0, 4.0, 2.0), 1e-9))
        assert(Matrix.elementwise_equal(result.normalizedMatrix(0), Array(1.0, 0.0, 0.8, 1.0, 1.0, 1.0 / 3), tol))
    }

    test("Rim - default ranges and ideals") {
        val result = rim(decmat, weights, directions)
        assertEquals(result.scores.length, 5)
        assert(result.scores.forall(s => s >= 0.0 && s <= 1.0))
    }
}

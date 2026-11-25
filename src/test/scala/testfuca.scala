import org.expr.mcdm.fuca
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

// Test data from JMcDM's testfuca.jl (Zakeri et al., 2024)
class TestFuca extends munit.FunSuite {
    test("Fuca - Zakeri et al. example") {
        val decmat = Array(
            Array(315.0, 110.0, 300.0, 205.0, 38.5, 2.2, 0.005, 2016.0),
            Array(600.0, 300.0, 350.0, 305.0, 28.0, 3.7, 0.005, 2016.0),
            Array(600.0, 300.0, 350.0, 305.0, 28.0, 3.7, 0.005, 1998.0),
            Array(600.0, 400.0, 380.0, 305.0, 28.0, 3.7, 0.005, 1992.0),
            Array(315.0, 110.0, 300.0, 205.0, 38.5, 2.2, 0.005, 2002.0),
            Array(315.0, 110.0, 300.0, 205.0, 38.5, 2.2, 0.002, 2009.0),
            Array(500.0, 200.0, 350.0, 205.0, 31.5, 3.7, 0.005, 2009.0),
            Array(510.0, 205.0, 355.0, 205.0, 31.5, 3.7, 0.005, 2014.0),
            Array(1280.0, 550.0, 600.0, 510.0, 53.5, 3.4, 0.002, 2017.0),
            Array(600.0, 500.0, 400.0, 355.0, 37.0, 3.7, 0.002, 2018.0),
            Array(1600.0, 720.0, 650.0, 510.0, 53.5, 4.2, 0.002, 2014.0),
            Array(510.0, 205.0, 355.0, 205.0, 31.5, 3.7, 0.005, 2016.0)
        )
        val weights = Array.fill(8)(0.125)
        val directions = Array.fill(8)(Maximize).updated(6, Minimize)

        val result = fuca(decmat, weights, directions)

        val expectedScores = Array(8.75, 6.4375, 7.3125, 6.75, 9.5, 8.5625, 8.1875, 7.25, 2.8125, 3.5, 2.0, 6.9375)
        val expectedRanks = Array(11, 4, 8, 5, 12, 10, 9, 7, 2, 3, 1, 6).map(_.toDouble)

        assert(Matrix.elementwise_equal(result.scores, expectedScores, 1e-9))
        assert(Matrix.elementwise_equal(result.ranks, expectedRanks, 1e-9))
        assert(Matrix.elementwise_equal(result.rankMatrix(0), Array(11.0, 11.0, 11.0, 9.5, 4.0, 11.0, 8.5, 4.0), 1e-9))
    }
}

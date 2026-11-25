import org.expr.mcdm.spotis
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestSpotis extends munit.FunSuite {
    test("Spotis - car selection with bounds") {
        val decmat = Array(
            Array(15000.0, 4.3, 99.0, 42.0, 737.0),
            Array(15290.0, 5.0, 116.0, 42.0, 892.0),
            Array(15350.0, 5.0, 114.0, 45.0, 952.0),
            Array(15490.0, 5.3, 123.0, 45.0, 1120.0)
        )

        val weights = Array(5.0, 4.0, 4.0, 1.0, 3.0).map(_ / 17.0)
        val directions = Array(Minimize, Minimize, Minimize, Maximize, Maximize)

        val result = spotis(
            decmat,
            weights,
            directions,
            Some(Array(14000.0, 3.0, 80.0, 35.0, 650.0)),
            Some(Array(16000.0, 8.0, 140.0, 60.0, 1300.0))
        )

        assert(Matrix.elementwise_equal(result.scores, Array(0.4779, 0.5781, 0.5558, 0.5801), 2e-4))
        assert(Matrix.elementwise_equal(result.ranks, Array(1.0, 3.0, 2.0, 4.0), 1e-9))
    }
}

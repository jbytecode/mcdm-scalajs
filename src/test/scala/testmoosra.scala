import munit.Assertions as A

import org.expr.mcdm.moosra
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestMoosra extends munit.FunSuite {
  test("Moosra Example - 1") {

    val decmat = Array(
      Array(25.0, 21, 19, 22),
      Array(65.0, 78, 53, 25),
      Array(7.0, 6, 5, 2),
      Array(20.0, 24, 33, 31)
    ).transpose

    val weights = Array(0.25, 0.25, 0.25, 0.25)

    val fns = Array(Maximize, Maximize, Minimize, Maximize)

    // Vector normalization per criterion, as in MOORA (cross-checked with pyDecision 5.1.8)
    val expectedscores = Array(2.2725788177322643, 2.815245881568694, 3.1745836984778255, 6.833640474697763)

    val expectedranks = Array(4.0, 3, 2, 1)

    val resultLeftRight = moosra(decmat, weights, fns)
    val result = resultLeftRight match {
      case Left(s) => fail(s"Moosra failed: $s")
      case Right(res) => res
    }

    A.assert(
        Matrix.elementwise_equal(
            result.scores,
            expectedscores,
            0.0001
        )
    )

    A.assert(
        Matrix.elementwise_equal(
            result.ranks,
            expectedranks,
            0.0001
        )
    )
  }

  test("Moosra published example: Adali and Isik (2017), Table 10") {
    // Laptop selection, J Ind Eng Int 13:229-237, doi:10.1007/s40092-016-0175-5
    val decmat = Array(
      Array(3.5, 6, 1256, 4, 16, 3, 17.3, 8, 2.82, 4100),
      Array(3.1, 4, 1000, 2, 8, 1, 15.6, 5, 3.08, 3800),
      Array(3.6, 6, 2000, 4, 16, 3, 17.3, 5, 2.9, 4000),
      Array(3.0, 4, 1000, 2, 8, 2, 17.3, 5, 2.6, 3500),
      Array(3.3, 6, 1008, 4, 12, 3, 15.6, 8, 2.3, 3800),
      Array(3.6, 6, 1000, 2, 16, 3, 15.6, 5, 2.8, 4000),
      Array(3.5, 6, 1256, 2, 16, 1, 15.6, 6, 2.9, 4000)
    )
    val weights = Array(0.297, 0.025, 0.035, 0.076, 0.154, 0.053, 0.104, 0.017, 0.025, 0.214)
    val fns = Array.fill(8)(Maximize) ++ Array(Minimize, Minimize)
    val published = Array(3.382, 2.509, 3.533, 2.860, 3.343, 3.184, 3.001)
    val result = moosra(decmat, weights, fns) match {
      case Left(s) => fail(s"Moosra failed: $s")
      case Right(res) => res
    }
    A.assert(Matrix.elementwise_equal(result.scores, published, 0.0006))
    A.assert(Matrix.elementwise_equal(result.ranks, Array(2.0, 7, 1, 6, 3, 4, 5), 0.0001))
  }
}

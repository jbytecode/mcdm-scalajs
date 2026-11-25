import munit.Assertions as A

import org.expr.mcdm.psi
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestPsi extends munit.FunSuite {
  test("Psi Example - 1") {

    val decmat = Array(
        Array(9.0, 8.0, 7.0),
        Array(7.0, 7.0, 8.0),
        Array(6.0, 9.0, 6.0),
        Array(7.0, 6.0, 6.0)
      ).transpose

    // Preference variation per criterion (cross-checked with pyDecision 5.1.8)
    val expected_scores = Array(0.8884920812205419, 0.9038022662539607, 0.8281521210464404)

    val w = Array(4.0, 2.0, 6.0, 8.0)

    val fns = Array(Maximize, Maximize, Maximize, Maximize)

    val result = psi(decmat, w, fns)

    A.assert(Matrix.elementwise_equal(result.scores, expected_scores, 1e-05), "Scores do not match in Psi")
    }

  test("Psi published examples: Petkovic et al. (2017), Tables 5 and 8") {
    // Facta Universitatis Ser. Mech. Eng. 15(1):97-106, doi:10.22190/FUME151120001P
    val decmat1 = Array(
      Array(0.061, 219.74, 5.8),
      Array(0.093, 3523.72, 6.3),
      Array(0.064, 2693.21, 6.8),
      Array(0.028, 761.46, 5.8),
      Array(0.034, 1593.48, 5.8),
      Array(0.013, 2849.15, 6.2)
    )
    val result1 = psi(decmat1, Array(1.0, 1, 1), Array(Minimize, Minimize, Minimize))
    A.assert(Matrix.elementwise_equal(result1.scores,
      Array(0.7851, 0.5492, 0.5333, 0.7226, 0.6724, 0.7949), 0.00006))
    A.assert(Matrix.elementwise_equal(result1.ranks, Array(2.0, 5, 6, 3, 4, 1), 0.0001))

    val decmat2 = Array(
      Array(0.035, 34.5, 847, 1.76, 0.335, 0.5, 0.59, 0.59),
      Array(0.027, 36.8, 834, 1.68, 0.335, 0.665, 0.665, 0.665),
      Array(0.037, 38.6, 808, 2.4, 0.59, 0.59, 0.41, 0.5),
      Array(0.028, 32.6, 821, 1.59, 0.5, 0.59, 0.59, 0.41)
    )
    val fns2 = Array(Minimize, Minimize, Minimize, Minimize, Maximize, Minimize, Minimize, Maximize)
    val result2 = psi(decmat2, Array.fill(8)(1.0), fns2)
    A.assert(Matrix.elementwise_equal(result2.scores, Array(0.8457, 0.8463, 0.8539, 0.8725), 0.00006))
    A.assert(Matrix.elementwise_equal(result2.ranks, Array(4.0, 3, 2, 1), 0.0001))
  }
}

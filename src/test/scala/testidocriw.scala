import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.idocriw

class TestIdocriw extends munit.FunSuite {

  test("Idocriw Example - 1") {

    val decmat = Array(
      Array(3.0, 100.0, 10.0, 7.0),
      Array(2.5, 80.0, 8.0, 5.0),
      Array(1.8, 50.0, 20.0, 11.0),
      Array(2.2, 70.0, 12.0, 9.0)
    )

    val cilos_w = Array(0.3343, 0.2199, 0.1957, 0.2501)

    val entropy_w = Array(0.1146, 0.1981, 0.4185, 0.2689)

    val idowric_w = Array(0.1658, 0.1886, 0.3545, 0.2911)

    val dirs = Array(Minimize, Maximize, Minimize, Maximize)

    val result = idocriw(decmat, dirs)

    A.assert(
      Matrix.elementwise_equal(
        result.entropy_weights,
        entropy_w,
        0.0001
      ),
      "Entropy weights are not equal in Idocriw"
    )

    A.assert(
      Matrix.elementwise_equal(
        result.cilos_weights,
        cilos_w,
        0.0001
      ),
      "Cilos weights are not equal in Idocriw"
    )

    A.assert(
      Matrix.elementwise_equal(
        result.weights,
        idowric_w,
        0.0001
      ),
      "Idocriw weights are not equal in Idocriw"
    )

  }
}
